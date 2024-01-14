---
title:                "Java recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

As developers, one of the most common tasks we do is debugging our code to find and fix errors. A helpful tool in this process is printing debug output, which allows us to see the values of certain variables and track the flow of our program. This can save us time and frustration in finding the root cause of a bug.

## How To

To print debug output in Java, we can use the `System.out.println()` method. This method takes in a string or variable and prints it to the console. Let’s take a look at an example:

```Java
int num1 = 5;
int num2 = 10;
int sum = num1 + num2;

System.out.println("The sum of " + num1 + " and " + num2 + " is " + sum);
```

Output:

```
The sum of 5 and 10 is 15
```

We can also use the `System.out.printf()` method to format our debug output. This method allows us to control the output format and also supports variables as input. Let’s see an example:

```Java
String name = "John";
int age = 30;

System.out.printf("Hi, my name is %s and I am %d years old.", name, age);
```

Output:

```
Hi, my name is John and I am 30 years old.
```

We can also print out the values of arrays and objects by using the `Arrays.toString()` and `Objects.toString()` methods respectively. These methods allow us to print out the contents of complex data structures for easier debugging.

## Deep Dive

Printing debug output can also be useful when working with methods and loops. By printing values within these code blocks, we can see the flow of our program and find any logic errors that may occur. For example, if we have a for loop that is supposed to sum up the values of an array, we can print out the values of the array and the current sum within the loop to ensure that the values are being added correctly.

Another useful technique when debugging is to use conditional statements to print out specific debug output only when certain conditions are met. This can save us from cluttering the console with irrelevant information.

In addition, many Java IDEs have their own built-in debuggers which allow us to set breakpoints and step through our code while also displaying the current values of variables. However, printing debug output can sometimes be a quicker and simpler option, especially for smaller projects.

## See Also

- [Java Debugging Tutorial](https://www.baeldung.com/java-debugging)
- [Debugging in Eclipse IDE](https://www.eclipse.org/community/eclipse_newsletter/2017/may/article1.php)
- [Debugging With IntelliJ](https://www.jetbrains.com/help/idea/debug.html)