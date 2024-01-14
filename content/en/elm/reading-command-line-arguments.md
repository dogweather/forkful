---
title:                "Elm recipe: Reading command line arguments"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Why

Are you tired of manually entering in data every time you run your Elm program? Look no further! By learning how to read command line arguments, you can automate this process and save yourself time and effort.

##How To

To read command line arguments in Elm, we will be using the `Platform` module. Within this module, there is a function called `Options` which allows us to access the command line arguments. Let's take a look at an example:

```Elm
import Platform exposing (Options)

main =
    Options.init
        |> Options.customize
            { args = 1 }
        |> Options.parse
            (\myArg -> processArgument myArg)

processArgument myArg =
    -- Do something with the argument
    ```

In this code, we first import the `Platform` module and specifically the `Options` function. Then, we use the `init` function to initialize the options and `customize` to specify how many arguments we are expecting (in this case, one). Lastly, we use the `parse` function to pass in a function that will handle the argument. In this case, we have named the argument `myArg` and will process it in the `processArgument` function.

Let's say we run this program with the argument `elm-blog`. The `processArgument` function would then be called with `myArg` equal to `"elm-blog"`. Pretty neat, right?

##Deep Dive

If you want to dig deeper into reading command line arguments, you can also customize the options further by specifying a default value, description, and type for each argument. Additionally, you can also specify flags and options, which are similar to arguments but have a different syntax.

Once you have parsed the arguments, you can use them in your program however you see fit. For example, you can use them to initialize variables or flags, or to execute specific functions based on user input.

##See Also

If you want to learn more about reading command line arguments in Elm, check out these resources:

- [Elm Platform Documentation](https://package.elm-lang.org/packages/elm/core/latest/Platform-Options)
- [Elm Programming Language: Learn to Code With Elm](https://elmprogramming.com/command-line-arguments.html)
- [Elm Town Podcast: Command Line Tools in Elm](https://elmtown.simplecast.com/episodes/2)