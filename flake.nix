{
  description = "The WTF compiler.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in
  {
    devShells."${system}".default = pkgs.mkShell {
      packages = with pkgs; [
        llvm_18
        llvmPackages_18.libllvm
        libxml2
        libffi
      ];

      env = {
        LLVM_SYS_180_PREFIX = "${pkgs.llvm_18.dev}";
        CFLAGS = "-L${pkgs.libxml2}/lib -L${pkgs.libffi}/lib";
      };
    };
  };
}
