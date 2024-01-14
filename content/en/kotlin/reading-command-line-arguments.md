---
title:    "Kotlin recipe: Reading command line arguments"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why
Command line arguments are a fundamental aspect of programming and understanding how to read them can greatly enhance your skills as a developer. In this article, we will explore the basics of reading command line arguments in Kotlin.

## How To
To start off, let's take a look at a simple example of how to read command line arguments in Kotlin:

```Kotlin
fun main(args: Array<String>) {
    println("The number of arguments is: ${args.size}")

    for (arg in args) {
        println(arg)
    }
}
```

This code snippet uses the `main` function and the `args` parameter, which is an array of strings containing the command line arguments. Using the `size` property, we can determine the number of arguments passed in. Then, by iterating through the `args` array, we can print out each argument on a new line.

Now, let's run this code with some sample arguments. If we run the command `kotlin Main.kt argument1 argument2`, the output should look like this:

```
The number of arguments is: 2
argument1
argument2
```

As shown in this example, reading command line arguments in Kotlin is relatively straightforward. All you need to do is access the `args` array in the `main` function and use it to manipulate the input as needed.

## Deep Dive
To delve deeper into reading command line arguments in Kotlin, let's take a look at how we can handle different types of arguments. One way to achieve this is by using `when` expressions. Let's take a look at an example:

```Kotlin
for (arg in args) {
    when (arg) {
        "hello" -> println("Hello there!")
        "bye" -> println("Goodbye!")
        else -> println("I don't understand.")
    }
}
```

In this code, we are checking each argument in the `args` array and determining the appropriate response based on its value. If the argument is "hello", we print a friendly message. If it is "bye", we say goodbye. And if it is anything else, we print a message saying we don't understand.

This is just one way to handle different types of arguments, but there are many other ways you can manipulate and process command line arguments in Kotlin. Experiment and find what works best for your project!

## See Also
- [Kotlin command line basics](https://kotlinlang.org/docs/command-line.html)
- [Kotlin documentation](https://kotlinlang.org/docs/reference/) 
- [Kotlin tutorial for beginners](https://www.freecodecamp.org/news/learn-kotlin-for-android-development/)