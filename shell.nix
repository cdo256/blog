{ pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    emacs
    hugo
    git
    pandoc
    emacsPackages.htmlize
    #emacsPackages.ox-publish
    emacsPackages.ox-hugo
  ];
}
