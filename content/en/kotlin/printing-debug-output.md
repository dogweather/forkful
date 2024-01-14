---
title:                "Kotlin recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why 
Debugging is an essential part of the software development process. It helps in identifying and fixing errors in the code, ensuring the program runs smoothly. One of the tools used in debugging is printing debug output. In this blog post, we will discuss the importance of printing debug output and how it can help in writing better code. So, if you are new to Kotlin programming or looking for ways to improve your debugging skills, this post is for you!

## How To
Printing debug output in Kotlin is a simple and effective way to understand how your code is running. Let's take a look at a few coding examples to see how it works:

```
fun main() {
  var num1: Int = 5
  var num2: Int = 10

  //printing debug output
  println("The value of num1 is: " + num1)
  println("The value of num2 is: " + num2)

  //calculating sum
  var sum = num1 + num2

  //printing debug output
  println("The sum of num1 and num2 is: " + sum)
}
```
Output:
```
The value of num1 is: 5
The value of num2 is: 10
The sum of num1 and num2 is: 15
```
In the above code, we have two variables `num1` and `num2` with initial values of 5 and 10 respectively. We then print out their values using `println()` statements to display the debug output. This helps us to understand the flow of our code and also to check if the values are being assigned correctly. Similarly, we can print out other variables or data structures to better understand our code.

Another way to print debug output is by using `Log` class from the `android.util` package. This is particularly useful when working with Android applications. Let's see an example:
```
fun printDebugOutput(msg: String) {
  Log.d("DEBUG", msg)
}
```
In the above function, we are using `Log.d()` to print out the debug output with a tag of "DEBUG". This can be helpful in filtering out specific output when debugging large applications.

## Deep Dive
Now that we have seen how to print debug output, let's take a deeper dive into its importance. Debug output not only helps in understanding the flow of code but also aids in identifying any errors or issues that may occur during runtime. By printing out different variables and data structures, we can track their values and pinpoint where the error is occurring. This can save us a lot of time and effort in trying to find the source of the error manually.

Moreover, printing debug output is also useful when working with complex algorithms or logic. It allows us to see how the values are changing at each step and helps in identifying any potential errors in the logic.

## See Also
- [Kotlin Official Documentation](https://kotlinlang.org/docs/home.html)
- [Debugging in Kotlin using IntelliJ IDEA](https://www.jetbrains.com/help/idea/debugging-code-in-intellij-idea.html)
- [Debugging in Kotlin using Android Studio](https://developer.android.com/studio/debug/)
- [Effective Debugging Techniques in Kotlin](https://medium.com/coding-with-flutter/effective-debugging-techniques-in-kotlin-466e99633c0e)