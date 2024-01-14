---
title:                "Kotlin recipe: Reading command line arguments"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Reading command line arguments can be a useful skill for any programmer, as it allows for more flexibility and customization when running a program. By learning how to read command line arguments, you can create more user-friendly programs and solutions that can handle a variety of inputs.

## How To
To read command line arguments in Kotlin, we can use the `args` variable that is automatically available in our program. This variable contains an array of strings, where each element represents a command line argument passed in when the program is run. Let's take a look at an example:

```kotlin
fun main(args: Array<String>) {
   println("Hello, " + args[0] + "!")
}
```

In this code, we are using the `args` variable to access the first command line argument passed in when running the program. We can then use this argument to customize the output of our program, as shown in the `println` statement.

To run this program, we would use the following command in the terminal:

```
kotlin MyProgram.kt World
```

The output would be:

```
Hello, World!
```

If no command line arguments are provided, the `args` variable will simply be an empty array.

## Deep Dive
Apart from accessing command line arguments using the `args` variable, Kotlin also provides a `CommandLine` class that can be used for more advanced command line operations. This class allows for parsing and handling of different types of arguments, such as flags, options, and positional arguments.

To use this class, we first create a `CommandLine` object by passing in the `args` array to its constructor. We can then use methods like `getOptionValue()` and `hasOption()` to retrieve and check for specific arguments. We can also set default values for arguments, and even create custom help messages for our program.

To learn more about the `CommandLine` class and its capabilities, refer to the official Kotlin documentation [here](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-command-line/).

## See Also
- [Kotlin documentation on command line arguments](https://kotlinlang.org/docs/reference/command-line.html)
- [Article on creating a command line program with Kotlin](https://medium.com/@pedrobgs/create-a-basic-command-line-tool-with-kotlin-104a0863a81c)
- [Tutorial on building a command line game using Kotlin](https://auth0.com/blog/command-line-programs-in-kotlin/)