---
title:                "Java recipe: Printing debug output"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an inevitable part of any programming journey, and it can be a frustrating process at times. As developers, it is our responsibility to find and fix any errors or bugs in our code. One way to make this process smoother and more efficient is by using debug output. Printing debug output lets us see what's happening in our code at certain points and helps us understand how our program is running. It can save us valuable time and effort, making it a crucial technique to learn for any programmer.

## How To

To start printing debug output in Java, we can use the built-in method `System.out.println()`. This method takes in a parameter and prints the value to the console. Let's take a look at an example:

```Java
int num1 = 10;
int num2 = 5;
System.out.println("The sum of " + num1 + " and " + num2 + " is " + (num1 + num2));
```

The output of this code would be: `The sum of 10 and 5 is 15`.

Alternatively, we can also use the `System.out.printf()` method to format the output. This method takes in a format string and corresponding variables to print out. For example:

```Java
String name = "John";
int age = 25;
System.out.printf("%s is %d years old.", name, age);
```

The output of this code would be: `John is 25 years old.`

We can also combine these methods for more complex debug output, such as printing out the value of variables at different points in our code. For instance:

```Java
int num1 = 5;
int num2 = 10;
System.out.println("num1 value: " + num1);
System.out.println("num2 value: " + num2);
num1 += 5;
num2 -= 5;
System.out.println("New num1 value: " + num1);
System.out.println("New num2 value: " + num2);
```

The output of this code would be:

```
num1 value: 5
num2 value: 10
New num1 value: 10
New num2 value: 5
```

## Deep Dive

While printing simple output like in the examples above can be helpful, we can also use debug output to dive deeper into our code's logic and find any errors. For instance, we can use `if` statements to only print certain output when a condition is met. Let's look at an example:

```Java
int num1 = 5;
int num2 = 10;
if (num1 < num2) {
    System.out.println("num1 is less than num2");
}
```

The output of this code would be: `num1 is less than num2`.

We can also use debug output in loops to see the values of variables each time the loop runs. This can be particularly useful when we have a loop that is not working as expected. Let's take a look at an example:

```Java
for (int i = 1; i <= 5; i++) {
    System.out.println("i value: " + i);
}
```

The output of this code would be:

```
i value: 1
i value: 2
i value: 3
i value: 4
i value: 5
```

This can help us understand how the loop is working and identify any mistakes we may have made.

## See Also

Here are some additional resources to help you master debugging in Java:

- [Debugging: The Art of Finding and Fixing Bugs](https://www.ccs.neu.edu/home/matthias/Debugging/DEBUG.html)
- [Debugging Tips and Tricks for Java Developers](https://stackify.com/debugging-tips-and-tricks-for-java-developers/)
- [Debugging in Java: A Practical Approach](https://www.baeldung.com/java-debugging)

So go ahead and start incorporating debug output into your coding process. It will save you time, headaches, and will ultimately make you a better programmer. Happy debugging!