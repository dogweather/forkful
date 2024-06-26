---
date: 2024-01-20 17:52:54.008715-07:00
description: 'How to: Let''s print stuff to the console.'
lastmod: '2024-03-13T22:45:00.051037-06:00'
model: gpt-4-1106-preview
summary: Let's print stuff to the console.
title: Printing debug output
weight: 33
---

## How to:
Let's print stuff to the console:

```Kotlin
fun main() {
    val magicNumber = 42
    println("The magic number is $magicNumber")

    debugPrint("The magic number squared equals ${magicNumber * magicNumber}")
}

fun debugPrint(message: String) {
    if (BuildConfig.DEBUG) {
        println("DEBUG: $message")
    }
}
```
Sample output:
```
The magic number is 42
DEBUG: The magic number squared equals 1764
```
Quick and dirty, you see what your values are right there in the console.

## Deep Dive
Printing to the console for debugging is old as the hills. It's simple, it's prevalent in all programming languages, and it gets the job done. But, it's not fancy, and in complex systems, too much output can be a mess.

Alternatives to `println` in Kotlin could be using logging frameworks like `Log4j` or Kotlin's built-in `Logging` utility which helps filter messages based on severity levels. 

A nuance in Kotlin, as seen in our `debugPrint` function, is to check if we're in a debug build; This way, we don't clutter production logs with our debug messages, keeping our actual deployments clean and user-friendly.

## See Also
- For an intro to logging in Kotlin, hit the official docs: [Kotlin Logging](https://github.com/MicroUtils/kotlin-logging)
- JetBrains’ take on debugging strategies: [IntelliJ IDEA Debugging](https://www.jetbrains.com/help/idea/debugging-code.html)
- If you’re using Android, the official guide on using Logcat is invaluable: [Android Logcat Documentation](https://developer.android.com/studio/command-line/logcat)
