{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  name = "stream-shell";
  buildInputs = with pkgs; [
    clojure
    leiningen
    openjdk
    ffmpeg
    coreutils
  ];
}
