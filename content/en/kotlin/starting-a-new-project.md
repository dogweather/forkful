---
title:    "Kotlin recipe: Starting a new project"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why

Starting a new project can seem daunting, but it is the first step towards creating something innovative and impactful. Whether you are a seasoned developer looking for a new challenge or a beginner looking to improve your skills, starting a new project is a great way to sharpen your coding abilities and showcase your creativity.

## How To

To begin, let's create a simple Kotlin project that prints "Hello, world!" to the console. Open your preferred code editor and create a new Kotlin file. In this example, we will name it "HelloWorld.kt".

```
Kotlin
fun main(args: Array<String>) {
    println("Hello, world!")
}
```

Now, let's dive into the code. The first line begins with the keyword "fun" which declares a function. In Kotlin, "main" is the entry point for our program. The "args" parameter allows us to pass command-line arguments to our program if needed. We then use the "println" function to output our desired message to the console. 

Next, we need to compile and run our program. In the terminal, navigate to the directory where your "HelloWorld.kt" file is located and run the following commands:

```
Kotlin
//compile the program
kotlinc HelloWorld.kt -include-runtime -d HelloWorld.jar

//run the program
java -jar HelloWorld.jar
```

If all goes well, you should see "Hello, world!" printed in the console. Congratulations, you have created your first Kotlin project!

## Deep Dive

Now that we have successfully created a Hello World program, let's take a deeper look at starting a new project in Kotlin. When starting a new project, it is important to choose a suitable code editor, understand the project structure, and familiarize yourself with the Kotlin syntax.

First, you can use any code editor for Kotlin, such as IntelliJ IDEA, Android Studio, or Visual Studio Code. These editors offer robust support for Kotlin, including code completion, debugging, and more.

Next, let's examine the project structure. Unlike Java, which requires a specific directory structure, Kotlin is more flexible. However, a typical project structure for Kotlin would include a "src" directory that contains subdirectories for your source code and resources, a "lib" directory for external libraries, and a "build" directory for generated files.

Finally, it is essential to understand Kotlin's syntax, which is similar to Java with a few improvements. One such improvement is the use of the "var" and "val" keywords for declaring variables. "var" is used for variables that can be reassigned, while "val" is used for variables that are immutable.

With these basics in mind, you are now ready to start exploring the vast possibilities of Kotlin!

## See Also

- [Kotlin Documentation](https://kotlinlang.org/docs/getting-started.html)
- [Kotlin Koans - Interactive Exercises to Learn Kotlin](https://play.kotlinlang.org/koans/overview)
- [Kotlin Bootcamp for Programmers - A Free Online Course by Google](https://developer.android.com/courses/kotlin-bootcamp/overview)