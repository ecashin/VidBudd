# VidBudd

The usage example below assumes a list of files has been created
for the background video frames.
E.g., for frames extracted by the ffmpeg command below,
such a file could be created with the subsequent commands.
An alternative is to use `/dev/null` as the (empty) file.

    ffmpeg -i $HOME/Downloads/PXL_20201217_140624500.mp4 \
        -vf scale=500:250 \
        $HOME/tmp/sl-water/sl-water-%07d.bmp
    bg_frames=$HOME/tmp/bg-frames.txt
    find $HOME/tmp/sl-water -name "*.bmp" | sort -n > "$bg_frames"

Usage example is shown below.

    fps=10; \
    wav=$HOME/DP-03/AKS-1-DirtBox.wav; \
    dotnet run "$fps" "$wav" /tmp/AKS $bg_frames \
      && ffmpeg -i $wav \
        -y -r $fps \
        -i /tmp/AKS-%04d.jpg \
        -c:v libx264 \
        -vf fps=$fps \
        -pix_fmt yuv420p \
        out.mp4 \
    && vlc out.mp4;