# VidBudd

Usage example is shown below.

    fps=10; \
    wav=$HOME/DP-03/AKS-1-DirtBox.wav; \
    dotnet run "$fps" "$wav" /tmp/AKS \
      && ffmpeg -i $wav \
        -y -r $fps \
        -i /tmp/AKS-%04d.jpg \
        -c:v libx264 \
        -vf fps=$fps \
        -pix_fmt yuv420p \
        out.mp4 \
    && vlc out.mp4;