# fused-effects-squeal ![Hackage](https://img.shields.io/hackage/v/fused-effects-squeal)

This is an overview of the way this library works. If you would like to learn how Squeal itself works you should head to the [morphismtech/squeal repository](https://github.com/morphismtech/squeal).

## Usage

There are two sepaeate effects with corresponding carriers: `Squeal` (with `SquealC`) and `SquealPool` (with `SquealPoolC`).

`Squeal` mimics the functions from `MonadPQ` (from `squeal-postgresql`) and represnts the "inside a transaction" effect.

You can run it directly with `runSquealWithConn` family of functions, but you probably want to use a connection pool.

`SquealPool` allows you to call `runSqueal` function, which picks a connection from the connection pool and runs the `Squeal` effect.
