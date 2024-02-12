---
title:                "Starting a new project"
aliases: - /en/kotlin/starting-a-new-project.md
date:                  2024-01-20T18:03:50.401549-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project means setting up the initial structure and vital files you’ll need for your application. Programmers initiate new projects to kick off development with a clean slate, tailored to their goals and technologies they plan to use.

## How to:

Let's get rolling with a Kotlin project using IntelliJ IDEA – a popular IDE for Kotlin.

1. Open IntelliJ IDEA.
2. Select `File > New > Project`.
3. Choose `Kotlin` on the left sidebar.
4. Specify your project's SDK (usually, IntelliJ will select the most recent one you've installed).
5. Pick a project template or stick with the default one.
6. Name your project and choose its location.
7. Hit `Finish`.

Boom, you've got a new Kotlin project. Your typical directory will look like this after creation:

```plaintext
projectName
|-- .idea
|-- src
     |-- main.kt
|-- build.gradle
```

And your `main.kt` might start off as simple as this:

```kotlin
fun main() {
    println("Ready, set, go Kotlin!")
}
```

Upon running the `main.kt`, you'll see:

```plaintext
Ready, set, go Kotlin!
```

## Deep Dive

Kotlin is a modern language that runs on the JVM (Java Virtual Machine), designed to be concise and safe. It was created by JetBrains and has been gaining traction, especially for Android app development, since announcing official support by Google in 2017.

Before jumping into a new Kotlin project, understand why you're choosing Kotlin:
- Concise syntax: Reduces boilerplate code.
- Interoperable with Java: Seamless integration with Java code and libraries.
- Smart casting: Less explicit type casting needed.
- Null-safety: Built-in system to avoid null pointer exceptions.

Alternatives to starting a new Kotlin project with IntelliJ IDEA:
- Command-Line: Create files manually, compile with `kotlinc`, and run using `kotlin` commands.
- Other IDEs: Use Android Studio for Android development or Eclipse with the Kotlin plugin.

When you start a new project in IntelliJ IDEA, it sets up the necessary Gradle configuration automatically. Gradle is a build automation system that manages dependencies, builds, and tests for your project. This setup allows you to import libraries, define module dependencies, and facilitate builds with ease.

## See Also

Want to go beyond the basics? Here's where to head next:

- Official Kotlin documentation: [Kotlin Docs](https://kotlinlang.org/docs/home.html)
- Gradle basics: [Gradle User Manual](https://docs.gradle.org/current/userguide/userguide.html)

Remember, the best way to learn is by doing. Start with the 'Hello World' and keep building from there. Happy coding!
