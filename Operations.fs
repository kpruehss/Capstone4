module Capstone4.Operations

open System
open Capstone4.Domain

/// Classify an account as either InCredit or Overdrawn
let classifyAccount account =
    if account.Balance >= 0M then
        (InCredit(CreditAccount account))
    else
        Overdrawn account

/// Withdraws an amount of an account (if there are sufficient funds)
let withdraw amount (CreditAccount account) =
    { account with
          Balance = account.Balance - amount }
    |> classifyAccount

let deposit amount account =
    let account =
        match account with
        | InCredit (CreditAccount account) -> account
        | Overdrawn account -> account

    { account with
          Balance = account.Balance + amount }
    |> classifyAccount

/// Try to parse user command into a BankOperation
let tryParseCommand command =
    match command with
    | "withdraw" -> Some Withdraw
    | "deposit" -> Some Deposit
    | _ -> None

/// Runs some account operation such as withdraw or deposit with auditing.
let auditAs operationName audit operation amount account accountId owner =
    let updatedAccount = operation amount account

    let transaction =
        { Operation = operationName
          Amount = amount
          Timestamp = DateTime.UtcNow }

    audit accountId owner.Name transaction
    updatedAccount

/// Creates an account from a historical set of transactions
let loadAccount (owner, accountId, transactions) =
    let openingAccount =
        classifyAccount
            { AccountId = accountId
              Balance = 0M
              Owner = { Name = owner } }

    transactions
    |> Seq.sortBy (fun txn -> txn.Timestamp)
    |> Seq.fold
        (fun account txn ->
            let operation = tryParseCommand txn.Operation

            match operation, account with
            | Some Deposit, _ -> account |> deposit txn.Amount
            | Some Withdraw, InCredit account -> account |> withdraw txn.Amount
            | Some Withdraw, Overdrawn _ -> account
            | None, _ -> account)
        openingAccount
