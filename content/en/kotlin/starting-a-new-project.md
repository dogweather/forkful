---
title:    "Kotlin recipe: Starting a new project"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project in any programming language can be both exciting and daunting. It allows you to create something from scratch and bring your ideas to life. With Kotlin, a modern, concise, and powerful language, you can take your project to new heights. So, let's dive into the hows of starting a new Kotlin project.

## How To

Kotlin provides a smooth and straightforward process for starting a new project. First, you need to set up your development environment with the latest version of IntelliJ IDEA. Once that's done, follow these steps to create a new Kotlin project:

1. Open IntelliJ IDEA and click on "New Project"
2. Select "Kotlin" as the primary language for your project and click "Next"
3. Choose the project boilerplate that best suits your needs: "Kotlin/JVM" for a project that runs on the Java Virtual Machine, "Kotlin/JS" for a project that runs on JavaScript, or "Kotlin/Multiplatform" for a project that can run on multiple targets
4. Give your project a name and choose a project location
5. Click "Finish" and voila, your new Kotlin project is ready to go!

To get you started, here's a simple Kotlin program that prints "Hello World!" to the console:

```Kotlin
fun main() {
    println("Hello World!")
}
```

And the output should be:

```
Hello World!
```

## Deep Dive

Now that you have a basic understanding of how to start a new Kotlin project, let's take a deeper dive into some key aspects. One important element to consider is the project structure. Kotlin projects typically follow a similar structure to Java projects, with source code located in the "src" folder and build files in the root directory. However, the main difference is that Kotlin files end with the ".kt" extension instead of ".java".

Another crucial aspect is adding external dependencies to your project. Kotlin has excellent interoperability with Java, which means you can use any Java library in your Kotlin project. The most common way to add dependencies is by using the built-in dependency management tool, Gradle. You can also use Maven or manually add the .jar files to your project.

## See Also

To learn more about Kotlin and how to start a new project, check out these useful resources:

- [Official Kotlin website](https://kotlinlang.org/)
- [Kotlin Tutorials](https://kotlinlang.org/docs/tutorials/)
- [Kotlin Koans - interactive learning exercises](https://kotlinlang.org/docs/tutorials/koans.html)
- [Kotlin Playground - try out code snippets in your browser](https://play.kotlinlang.org/)

So, what are you waiting for? Start your new Kotlin project today and see what you can create!