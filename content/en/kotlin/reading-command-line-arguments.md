---
title:                "Reading command line arguments"
html_title:           "Kotlin recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Reading command line arguments is an essential skill for any programmer, as it allows for the customization and flexibility of their code. Whether it's for creating a command line tool or a user-friendly application, understanding how to read and process command line arguments is crucial.

## How To

Reading command line arguments in Kotlin is a straightforward process. To get started, we first need to access the command line arguments array using the `args` parameter in the `main()` function. We can then use a simple `for` loop to iterate through the arguments and process them accordingly.

```
Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        // process each argument here
    }
}
```

Let's take a look at a simple example. Suppose we want to create a program that greets the user with their name and a custom message. We can achieve this by passing in the name and message as command line arguments and using string interpolation to display the desired output.

```
Kotlin
fun main(args: Array<String>) {
    val name = args[0]
    val message = args[1]
    println("Hello $name! $message")
}
```

If we run this program with the command line arguments `John` and `Welcome to our program!`, the output would be `Hello John! Welcome to our program!`

We can also use conditional statements to add more functionality to our program based on the arguments supplied. For example, we can use an `if` statement to check if a specific argument is present and then perform a specific action accordingly.

```
Kotlin
fun main(args: Array<String>) {
    if ("-h" in args) {
        println("This program provides a personalized greeting.")
    } else {
        val name = args[0]
        val message = args[1]
        println("Hello $name! $message")
    }
}
```

In the above example, if the argument `-h` is present, the program will display a helpful message instead of the personalized greeting.

## Deep Dive

Apart from the examples shown above, there are many other ways to read and process command line arguments in Kotlin. Some other approaches include using the `args.asList()` function to convert the arguments array into a list, using the `Arrays.copyOfRange()` function to extract a subset of arguments, and using the `args.indexOf()` function to search for a specific argument.

It's also worth noting that command line arguments in Kotlin are not limited to just strings. We can also pass in other data types such as integers, booleans, and even arrays.

To further explore the possibilities and intricacies of working with command line arguments in Kotlin, check out the official documentation and try implementing some more complex examples.

## See Also

- [Kotlin documentation on command line arguments](https://kotlinlang.org/docs/command-line.html)
- [Tutorial on creating command line applications in Kotlin](https://www.baeldung.com/kotlin-command-line-programming)