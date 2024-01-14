---
title:    "Haskell recipe: Reading command line arguments"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Have you ever wondered how to access and use command line arguments in your Haskell code? In this blog post, we will show you how to do just that! Understanding how to read command line arguments can be incredibly useful for creating command line applications, implementing user input in your programs, and more.

## How To

Let's dive right in and see how we can read command line arguments in Haskell.

```Haskell
import System.Environment

main = do
    -- Get the command line arguments
    args <- getArgs

    -- Print the arguments
    putStrLn "Command line arguments:"
    mapM_ print args
```

Assuming we have saved this file as "commandlineargs.hs", we can compile and run it from the command line as follows:

```bash
$ ghc commandlineargs.hs
$ ./commandlineargs argument1 argument2 argument3
Command line arguments:
"argument1"
"argument2"
"argument3"
```

As you can see, the "getArgs" function from the "System.Environment" module allows us to access the command line arguments and store them in a list. We can then use this list for further processing in our program. In the above example, we simply printed out the arguments, but you can use them for a variety of purposes.

Now, what if you want to pass in flags or options as command line arguments? Here's an example of how you can do that using the "getOpt" function from the "System.Console.GetOpt" module.

```Haskell
import System.Console.GetOpt

-- Define possible options and their actions
options :: [OptDescr (String -> String)]
options = [Option "n" ["name"] (ReqArg (\s -> "Hello " ++ s) "NAME") "Print a greeting with name"]

main = do
    -- Get the command line arguments
    (opts, args, _) <- getOpt Permute options <$> getArgs

    -- Apply the options to the arguments
    let args' = map (\f -> f "") opts ++ args

    -- Print the arguments
    putStrLn "Command line arguments with options:"
    mapM_ print args'
```

Let's compile and run this program with some options and arguments:

```bash
$ ghc commandlineargs.hs
$ ./commandlineargs -n John argument1 argument2
Command line arguments with options:
"Hello John"
"argument1"
"argument2"
```

As you can see, the options we specified were applied to the arguments in our program. You can use this functionality to create more dynamic and customizable command line applications.

## Deep Dive

Now, let's take a deeper look at how command line arguments are handled in Haskell. When we run a Haskell program from the command line, the arguments are passed in as a list of strings. This list is made available to our program through the "getArgs" function. We can then use functions like "head" and "tail" to access specific arguments, or we can use "map" to apply a function to each argument in the list.

Additionally, the "getOpt" function allows us to specify different options and their corresponding actions. This gives us more control over how our program handles different command line inputs.

## See Also

- [Official Haskell Documentation on System.Environment](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)
- [Official Haskell Documentation on System.Console.GetOpt](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Console-GetOpt.html)
- [Stack Overflow thread on command line arguments in Haskell](https://stackoverflow.com/questions/7135835/command-line-arguments-haskell)