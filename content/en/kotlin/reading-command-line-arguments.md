---
title:                "Kotlin recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you are new to Kotlin, you may have heard about command line arguments but aren't quite sure what they are or why they are important. Simply put, command line arguments allow you to pass data to your program when it is executed. This can be extremely useful when you want to customize the behavior of your program without having to constantly modify the code.

## How To

To read command line arguments in Kotlin, we use the `args` parameter in the `main()` function. This parameter is an array of strings that contains the arguments passed when the program is executed.

Let's take a look at a simple example:

```Kotlin
fun main(args: Array<String>) {
    println("The following arguments were passed:")
    for (arg in args) {
        println(arg)
    }
}
```

If we execute this program with the command `kotlin Main.kt argument1 argument2`, the output would be:

```
The following arguments were passed:
argument1
argument2
```

As you can see, the arguments are stored in the `args` array and can be accessed using a for loop.

You can also access individual arguments directly using their index in the array, for example `args[0]` would give you the first argument in the array.

## Deep Dive

Command line arguments in Kotlin are not limited to just single words or numbers. You can also pass in strings or even multiple values by separating them with spaces.

Additionally, you can use flags or options to pass in specific parameters. These are typically preceded by a hyphen, such as `-username` or `-port`.

Keep in mind that the order of the arguments matters. For example, if you were expecting a string for the first argument and an integer for the second, make sure you pass them in that order when executing the program.

## See Also

For more information on command line arguments in Kotlin, check out the following resources:

- [Official Kotlin documentation on command line arguments](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Tutorial on using command line arguments in Kotlin](https://www.tutorialkart.com/kotlin/command-line-arguments-kotlin/)
- [Video tutorial on reading command line arguments in Kotlin](https://www.youtube.com/watch?v=eoy5EXKSGvw)

Happy coding!