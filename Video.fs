module Video

open System.Collections.Generic
open SixLabors.ImageSharp
open SixLabors.ImageSharp.ColorSpaces
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.Drawing.Processing

let defaultBackground wd ht =
    use bgImg = new Image<Rgba32>(wd, ht)
    let bgColor = Color(Rgba32(50uy, 50uy, 75uy))
    let imgRect = RectangleF(0f, 0f, float32 wd, float32 ht)
    bgImg.Mutate(fun i -> i.Fill(bgColor, imgRect) |> ignore)
    bgImg

// load images generated with ...
// ffmpeg -i ~/Downloads/PXL_20201217_140624500.mp4 -vf scale=500:250 ~/tmp/sl-water/sl-water-%07d.bmp
let loadBackgroundImage (frameFile: string) : Image<Rgba32> =
        Image.Load(frameFile)

let darken (amount: float) (img: Image<Rgba32>) =
        let rect = RectangleF(0.0f, 0.0f, float32 img.Width, float32 img.Height)
        let v = 0uy
        let alpha = System.Math.Round(amount * 255.0) |> byte
        img.Mutate(fun i -> i.Fill(Color(Rgba32(v, v, v, alpha)), rect) |> ignore)

let allBinsRects (img: Image<Rgba32>) (powers: float []) (powerSum: float) =
        let nBins = powers.Length
        let binWd = img.Width / nBins
        // let img = bg.Clone(fun i -> i |> ignore)
        let amp = powerSum * 255.0
        for j in 0..(nBins - 1) do
                let v = System.Math.Round(powers.[j] * amp) |> byte
                // let v = 255uy
                let hoff = j * binWd
                let rect = RectangleF(float32 hoff, 0.0f, float32 binWd, float32 img.Height)
                img.Mutate(fun i -> i.Fill(Color(Rgba32(v, v, v, v)), rect) |> ignore)

let makeStarAdder (wd, ht) (chunkPowers: float [][]) (chunkAmps: float []) : (Image<Rgba32> -> int -> unit) =
        let positions = new Stack<(float * float) []>()
        let nx = 5
        let ny = 2
        let offX = wd / (nx + 1)
        let offY = ht / (ny + 1)
        let resetPositions () =
                positions.Clear()
                positions.Push([|
                        for i in 1..nx do
                                for j in 1..ny do
                                (
                                        float (i * offX),
                                        float (j * offY)
                                )
                |])
        resetPositions ()
        printfn "orig positions: %A" (positions.Peek())
        let normal = Accord.Statistics.Distributions.Univariate.NormalDistribution()
        let rnorm (chunk: int) = 8.0 * chunkAmps.[chunk] * normal.Generate()
        let confined (p: (float * float) []) =
                let round (x: float) = System.Math.Round(x) |> int
                Array.map (fun (x: float, y: float) ->
                        (
                                System.Math.Clamp(round x, 0, wd),
                                System.Math.Clamp(round y, 0, ht)
                        )
                ) p
        let addStar (img: Image<Rgba32>) (i: int) =
                if chunkAmps.[i] < 0.3 then
                        resetPositions ()
                else if chunkAmps.[i] < 0.7 && positions.Count > 0 then
                        positions.Pop() |> ignore
                let lastPos = positions.Peek()
                let pos = Array.map (fun (x, y) ->
                        (x + (rnorm i), y + (rnorm i))) lastPos
                positions.Push(pos)
                for (x, y) in (confined pos) do
                        // printfn "img wd:%d ht:%d x:%f y:%f" img.Width img.Height x y
                        let star = Star(float32 x, float32 y, 5, float32 10.0, float32 30.0)
                        img.Mutate(fun i -> i.Fill(Color.Red, star) |> ignore)
        addStar

let makeWiperAdder (wd: int, ht: int) (audio: Audio.Track) =
        let colorConverter = Conversion.ColorSpaceConverter()
        let pathQueue = Queue<PointF [] * Rgb>()
        let queueMax = 15
        let mutable theta = System.Math.PI / 2.0
        let penWd = 3.0
        let startPoint = PointF(float32 penWd, float32 (float ht - penWd))
        let entropies = audio.PowerEntropies.[0] |> (Audio.scaleToRange -100.0 100.0) |> Array.map float32
        let smoothedAmps =
                [| audio.PowerSumsRaw.[0] |]
                |> Audio.smoothed 5
                |> Array.head
                |> Audio.scaleToRange -100.0 100.0
                |> Array.map float32
        let midRangeAmps =
                audio.FreqRangePowerSums(0, 750.0, 4000.0)
                |> Audio.scaleToRange -100.0 100.0
                |> Array.map  float32
        let addWiper (img: Image<Rgba32>) (chunk: int) =
                let endX = System.Math.Cos(theta) * (float wd * 2.0)
                let endY = (float ht * 2.0) - (System.Math.Sin(theta) * (float ht * 2.0))
                let clampedEndX = System.Math.Clamp(endX, penWd, float wd - penWd)
                let clampedEndY = System.Math.Clamp(endY, penWd, float ht - penWd)
                let endPoint = PointF(float32 clampedEndX, float32 clampedEndY)
                let points = [|startPoint; endPoint|]
                // let color = Color(Rgba32(0uy, 0uy, 0uy, alpha))
                let cieLabColor = CieLab(entropies.[chunk], smoothedAmps.[chunk], midRangeAmps.[chunk])
                let rgbColor = colorConverter.ToRgb(&cieLabColor)
                pathQueue.Enqueue((points, rgbColor))
                if pathQueue.Count > queueMax then
                        (pathQueue.Dequeue()) |> ignore
                theta <- theta - System.Math.PI / 360.0
                if theta <= 0.0 then
                        theta <- System.Math.PI / 2.0
                for (j, (points, rgbColor)) in seq { for points in pathQueue do points } |> Seq.indexed do
                        let alpha = 55 + 200 * ((j + 1) / pathQueue.Count) |> byte
                        let f v = (v * 255.0f) |> byte
                        let color = Color(Rgba32(f rgbColor.R, f rgbColor.G, f rgbColor.B, alpha))
                        img.Mutate(fun i -> i.DrawLines(color, float32 penWd, points) |> ignore)
        addWiper