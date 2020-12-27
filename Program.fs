open SixLabors.ImageSharp
open System.IO
open Accord.DirectSound

let outputWidth = 1000
let outputHeight = 500

let prefixFileName (path: string) (prefix: string) =
        sprintf "%s-%s" prefix (Path.GetFileName(path))

let genFreqImages (audio: Audio.Track) (outPathPrefix: string) (bgFrameFiles: seq<string>) (powerSums: float []) (chunkPowers: float [] []) =
        let wd = outputWidth
        let ht = outputHeight
        let defaultBg = Video.defaultBackground wd ht
        let fNam i = sprintf "%s-%04d.jpg" outPathPrefix i
        let addWiper = Video.makeWiperAdder (wd, ht) audio
        let addStars = Video.makeStarAdder (wd, ht) chunkPowers powerSums
        let mutable bgFiles = List.ofSeq bgFrameFiles
        for i in 0..(chunkPowers.Length - 1) do
                let img =
                        match bgFiles with
                        | [] -> defaultBg.Clone()
                        | (hd::tail) ->
                                bgFiles <- tail
                                Video.loadBackgroundImage hd
                Video.darken 0.5 img
                Video.allBinsRects img chunkPowers.[i] powerSums.[i]
                addWiper img i
                addStars img i
                img.Save(fNam i)

let genVidForAudio (chan: int) (audio: Audio.Track) (outPathPrefix: string) (bgFrameFiles: seq<string>) =
        genFreqImages
                audio
                outPathPrefix
                bgFrameFiles
                (audio.PowerSums.[chan]
                        |> (Audio.scaleToRange 0.0 1.0)
                        |> (Audio.sqrtize 2))
                (audio.Powers.[chan]
                        |> Array.map (fun x -> x |> (Audio.scaleToRange 0.0 1.0) |> (Audio.sqrtize 2))
        )

let readLines (fileName: string) =
        seq {
                use sr = new StreamReader(fileName)
                while not sr.EndOfStream do
                        yield sr.ReadLine()
        }

[<EntryPoint>]
let main argv =
        match argv with
        | [|fps; wavPath; outPathPrefix; bgFramesFileName|] ->
                use wfas = new WaveFileAudioSource(wavPath) // ... just to reference DirectSound
                let audio = Audio.Track(wavPath, int fps)
                printfn "Audio signal from %s" wavPath
                printfn "  has %d channels" audio.Signal.Channels
                printfn "  sample format: %A" audio.Signal.SampleFormat
                printfn "  sample rate: %d" audio.Signal.SampleRate
                printfn "  number of samples: %d" audio.Signal.Samples
                genVidForAudio 0 audio outPathPrefix (readLines bgFramesFileName)
        | _ -> failwith "Must have exactly two arguments."
        0