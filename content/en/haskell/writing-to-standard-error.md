---
title:    "Haskell recipe: Writing to standard error"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Writing to standard error is a useful skill for any Haskell programmer. It allows you to display error messages and debugging information while your program is running.

## How To

To write to standard error in Haskell, you can use the `hPutStrLn` function from the `System.IO` module. This function takes two arguments, the first being the handle to write to (in this case, `stderr`) and the second being the string you want to write.

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Something went wrong!"
```

This will print the string "Something went wrong!" to the standard error output when the program is run. 

As an example, let's create a simple program that tries to divide two numbers and prints an error message if the second number is zero.

```Haskell
import System.IO

main :: IO ()
main = do
  putStrLn "Enter two numbers:"
  xStr <- getLine
  yStr <- getLine
  let x = read xStr :: Double
  let y = read yStr :: Double
  if y == 0
    then hPutStrLn stderr "Error: cannot divide by zero!"
    else print (x / y)
```

Sample output:

```
Enter two numbers:
10
0
Error: cannot divide by zero!
```

## Deep Dive

In Haskell, there are three standard output channels: `stdout`, `stderr`, and `stdin`. Each of these is represented by a handle, which is an abstract data type used to interact with input/output devices.

Using `hPutStrLn` to write to standard error is similar to using `putStrLn` to write to standard output. However, the main difference is that the standard error output is typically used for displaying error messages and debugging information, while the standard output is used for regular program output.

It's important to note that standard error is not the same as an exception or error in Haskell. If your program encounters a runtime error, it will be displayed on the standard error output by default. However, you can also use `hPrint` to explicitly print exceptions to the standard error output.

## See Also

- [Haskell IO Documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Haskell Error Handling](https://wiki.haskell.org/Error_handling)