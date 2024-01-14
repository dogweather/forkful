---
title:                "Haskell recipe: Reading command line arguments"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Reading command line arguments is a crucial skill for any programmer who wants to create powerful and user-friendly command line applications. By being able to read arguments provided by the user, you can make your application more flexible and customizable.

## How To

In Haskell, reading command line arguments is a straightforward process. The `getArgs` function from the `System.Environment` module allows you to retrieve a list of all the arguments provided by the user. Let's see an example:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Arguments provided: " ++ show args)
```

In this code, we import the `System.Environment` module and then use the `getArgs` function to retrieve the arguments and store them in the `args` variable. Finally, we use the `putStrLn` function to print out the list of arguments to the console.

If we compile and run this code with the following command:

```
runhaskell args.hs hello world 123
```

We will get the following output:

```
Arguments provided: ["hello", "world", "123"]
```

As you can see, the `getArgs` function returns a list of strings, where each string represents a different argument provided by the user.

## Deep Dive

Now let's take a deeper look at how the `getArgs` function actually works. When you run a Haskell program from the command line, all the arguments provided after the name of the program are passed to the `main` function as a list of strings. Haskell then handles these arguments and makes them available to your program via the `getArgs` function.

Keep in mind that the `getArgs` function can only retrieve arguments that come after the name of the program. Any arguments provided before the name of the program will not be accessible.

Also, it's important to note that the `getArgs` function returns a pure list, meaning it's evaluated lazily. This means that if you only use a few arguments from the list, Haskell will only evaluate those arguments, resulting in better memory usage and performance.

## See Also

- [System.Environment module documentation](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html)
- [Haskell command line arguments tutorial](https://www.tutorialspoint.com/haskell/haskell_command_line_arguments.htm)
- [Lazy evaluation in Haskell](https://wiki.haskell.org/Lazy_evaluation)