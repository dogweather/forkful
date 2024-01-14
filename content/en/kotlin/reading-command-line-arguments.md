---
title:    "Kotlin recipe: Reading command line arguments"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Have you ever wondered how some programs are able to take in arguments from the command line? Maybe you're working on a project that requires this functionality. In this blog post, we'll explore how to read command line arguments in Kotlin and how it can be useful in your programming journey.

## How To
Reading command line arguments in Kotlin is a straightforward process. First, we need to import the necessary package:

```Kotlin
import kotlin.system.exitProcess
```

Next, we can use the `args` parameter in our `main()` function to access the arguments passed in from the command line:

```Kotlin
fun main(args: Array<String>) {
    // program logic goes here
}
```

Inside the `main()` function, we can use a simple `for` loop to iterate over the `args` array and print out each argument:

```Kotlin
for (arg in args) {
    println(arg)
}
```

If we run the program with arguments, we will see the output of those arguments in the console. For example, if we run our program with the arguments `hello world`:

```
hello
world
```

We can also add logic to handle situations where no arguments are passed in. For example, we can use the `isEmpty()` method to check if the `args` array is empty and display an error message if it is:

```Kotlin
if (args.isEmpty()) {
    println("No arguments provided. Please try again.")
    exitProcess(0)
}
```

We can then run our program without any arguments and see our error message displayed in the console.

## Deep Dive
One of the main benefits of reading command line arguments is the ability to make our programs more dynamic. Instead of hard-coding values in our code, we can pass them in as arguments and change them as needed.

We can also use command line arguments to control the flow of our programs. For example, we can pass in a specific argument to trigger a specific action in our program.

Another useful aspect of reading command line arguments is the ability to provide different types of input. We can pass in strings, numbers, and even flags (e.g. `--verbose`) to customize our program's behavior.

## See Also
- [Kotlin command line arguments documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-array/index.html)
- [Kotlin tutorials at JetBrains Academy](https://hyperskill.org/learn/step/10768)

Hopefully, this blog post has given you a better understanding of how to read command line arguments in Kotlin and how it can be helpful in your programming journey. Happy coding!