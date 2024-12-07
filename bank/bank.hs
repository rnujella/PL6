module Bank where

-- Define the BankOp newtype for stateful operations
newtype BankOp a = BankOp (Float -> (a, Float))

-- Functor instance for BankOp
instance Functor BankOp where
    fmap f (BankOp g) = BankOp (\s -> let (a, s') = g s in (f a, s'))

-- Applicative instance for BankOp
instance Applicative BankOp where
    pure x = BankOp (\s -> (x, s))
    (BankOp f) <*> (BankOp g) = BankOp (\s -> 
        let (h, s') = f s
            (a, s'') = g s'
        in (h a, s''))

-- Monad instance for BankOp
instance Monad BankOp where
    return = pure
    (BankOp f) >>= g = BankOp (\s -> 
        let (a, s') = f s
            BankOp h = g a
        in h s')

-- Deposit operation: adds the amount to the balance
deposit :: Float -> BankOp ()
deposit amount = BankOp (\balance -> ((), balance + amount))

-- Withdraw operation: deducts the amount from the balance
-- Ensures overdraft is limited to -100
withdraw :: Float -> BankOp Float
withdraw amount = BankOp (\balance -> 
    if balance - amount >= -100
    then (amount, balance - amount)
    else let actual = balance + 100 in (actual, balance - actual))

-- Get the current balance
getBalance :: BankOp Float
getBalance = BankOp (\balance -> (balance, balance))

-- Run a BankOp computation with an initial balance of 0
runBankOp :: BankOp a -> a
runBankOp (BankOp f) = fst (f 0)
