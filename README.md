# Uart Module

## Dependancies

- Nix Package Manager
    - Windows: Recommend Using the [NixOS WSL](https://nix-community.github.io/NixOS-WSL/install.html) image  
    - MacOS: [MacOS Nix Package Manager Setup](https://nixos.org/download/#nix-install-macos)
    - Other Linux Distributions: [Linux Nix Package Manager Setup](https://nixos.org/download/#nix-install-linux)

## Setup

```bash
git clone [url]
cd [folder]
sh dev_shell.sh
```

## Usage
**Note:** All build artifacts will be generated in the "out" folder

Generate Verilog: `make verilog`

Run Tests: `make test`

Generate Coverage: `make cov`

Run Synthesis: `make synth`

Run STA: `make sta`

## 

### Defining UART Parameters

To configure the uart, create an instance of `UartParams` with the desired parameters.

```scala
val uartParams = UartParams(
          dataWidth = 32,
          addressWidth = 32,
          wordWidth = 8,
          maxOutputBits = 8,
          syncDepth = 2,
          bufferSize = 1024,
          maxBaudRate = 25_000_000,
          maxClockFrequency = 25_000_000,
          coverage = true,
          verbose = true
        )
```

### Instantiating the Timer Module

Instantiate the `Uart` module with the defined parameters.

```scala
val uart = Module(new Uart(uartParams))
```
