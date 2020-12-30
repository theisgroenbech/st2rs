# From session types to Rust
In this project, we create a translation between security session types and Rust-code. We take advantage of session types for communication and describe the behaviour of each participant. In this project, we extend security session types further and introduce a translation to the Rust programming language. The protocol itself will be translated into runnable rust code while symbolic functions will be compiled to unimplemented functions, that the developer will then have to take care of implementing.

## Building
```bash
cd src
make
```
## Running
```bash
./main.native [FILE] rust
```
