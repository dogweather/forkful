---
title:    "Haskell recipe: Reading command line arguments"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Have you ever used a program on your computer and were asked to enter additional information through the command line? Most likely, the program was reading command line arguments to perform a specific action. In this blog post, we will explore how to read command line arguments in Haskell and why it is important for certain programs.

## How To

To read command line arguments in Haskell, we will need to use the `System.Environment` module. This module allows us to access the arguments passed when executing the program. Let's look at a simple example:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "The arguments are: " ++ show args
```

In this code, we import the `System.Environment` module and then use the `getArgs` function to retrieve the command line arguments. These arguments are stored in a list, which we can then manipulate as necessary. In this example, we simply print out the arguments using `putStrLn` and `show`.

Now let's try running our program with some arguments:

```
$ runghc myprogram.hs argument1 argument2
```

The output should be:

```
The arguments are: ["argument1", "argument2"]
```

As you can see, we have successfully read and accessed the command line arguments in our Haskell program.

## Deep Dive

There are a few things to keep in mind when reading command line arguments in Haskell. First, the `getArgs` function returns a list of strings. This means that we will need to convert these strings into the appropriate data type if we want to perform calculations or manipulations on them. Additionally, the order of the arguments passed in will determine the order in which they appear in the list.

Another important aspect to consider is how to handle input errors. For example, what should happen if a user forgets to provide an argument, or provides an argument in an incorrect format? It is important to anticipate and handle these errors in order to create a more robust and user-friendly program.

## See Also

- Haskell documentation for `System.Environment` module:
https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html
- Additional command line argument examples in Haskell:
https://www.stackbuilders.com/tutorials/haskell/command-line-arguments/

Now that you have a basic understanding of how to read command line arguments in Haskell, you can explore more advanced usage and techniques. Happy coding!