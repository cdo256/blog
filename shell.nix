{ pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.emacs
    pkgs.hugo
    pkgs.git
  ];
}