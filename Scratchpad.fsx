#load "Domain.fs"
#load "Operations.fs"

open Capstone4.Operations
open Capstone4.Domain
open System

type BankOperation =
    | Deposit
    | Withdraw
    | Exit

let tryParseCommand command =
    match command with
    | 'w' -> Some Withdraw
    | 'd' -> Some Deposit
    | 'x' -> Some Exit
    | _ -> None

tryParseCommand 'e'
