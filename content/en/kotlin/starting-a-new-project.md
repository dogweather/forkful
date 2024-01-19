---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starting a New Kotlin Project Simplified

## What & Why?

Initiating a new project is setting up the groundwork of your coding process. Programmers do it to jump-start their development, streamline workflow, and manage components efficiently.

## How to:

Let's initiate a new Kotlin project. Boot up IntelliJ IDEA and follow the steps:

`File -> New -> Project -> Kotlin -> JVM | IDEA -> Next -> Finish `

For a basic HelloWorld application, use the following coding snippet:

```Kotlin
fun main() {
    println("Hello World!")
}
```

Run the application. You should see the following output:

```Kotlin
Hello World!
```

The `fun main()` function is the entry point of a Kotlin application. The `println()` function prints the provided string to the console.

## Deep Dive

Starting a new project isn't exclusive to Kotlin or even modern languages. It's been a standard practice ever since the first procedural languages came into existence.

To keep things light, IDEs (Integrated Development Environments) like IntelliJ, Eclipse, or NetBeans provide options to create new projects. And if you love the terminal, CLI (Command Line Interface) tools like **gradle** or **maven** have got your back.

Certain specifics vary depending on the project type. For instance, while starting a new project in Kotlin, you'll notice a `src` folder with `Main.kt` file. It's a convention rather than a compulsory rule, and it stands for "Source", the primary directory for your Kotlin source files. `Main.kt` is the default main file where the execution of the Kotlin project begins.

## See Also

Further Reading and Extendable Knowledge:

1. [Kotlin Official Documentation](https://kotlinlang.org/docs/home.html)
2. [Starting New Project - IntelliJ IDEA](https://www.jetbrains.com/help/idea/creating-and-running-your-first-kotlin-application.html)
3. [Kotlin for Java Developers - Coursera Course](https://www.coursera.org/learn/kotlin-for-java-developers)
4. [Kotlin Programming - Big Nerd Ranch Guide](https://www.bignerdranch.com/books/kotlin-programming-the-big-nerd-ranch-guide/)