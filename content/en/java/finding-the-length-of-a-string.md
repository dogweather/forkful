---
title:                "Java recipe: Finding the length of a string"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As a programmer, it is important to have a strong understanding of the basic operations and functions in your preferred language. A common task in many programming languages is finding the length of a string. This may seem like a simple task, but it is a fundamental skill that can be applied to a variety of programs and projects.

## How To

To find the length of a string in Java, we will be using the built-in method `length()`. This method is part of the `String` class and is used to return the number of characters in a given string. Let's look at an example:

```Java
String message = "Hello World";
System.out.println(message.length());
```

In this code, we have declared a string variable `message` with the value of "Hello World". Then, using the `length()` method, we are able to print the number of characters in our string, which in this case is 11. You can also assign the length to a variable for later use, like this:

```Java
int length = message.length();
```

Now, let's take a look at a more complex example:

```Java
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        System.out.println("Enter a word or phrase:");
        String userInput = input.nextLine();

        System.out.println("The length of your string is: " + userInput.length());
    }
}
```

In this code, we are using the `Scanner` class to take user input and store it in the `userInput` variable. Then, we are using the `length()` method to find the length of the user's input and printing it out for them to see. You can modify and play around with this code to enhance your understanding of finding the length of a string in Java.

## Deep Dive

The `length()` method is actually a wrapper function for the `length` property, which is a variable of type `int`. This property stores the length of the string, and the `length()` method simply returns the value of this property. One important thing to note is that the `length()` method does not include the null character (`\0`) in its count. This means that if you have a string with 5 characters, but the last character is a null character, the `length()` method will return 4.

Additionally, the `length()` method is case-sensitive, meaning it will count uppercase and lowercase letters as different characters. So, the string "Hello" will have a length of 5, while "hello" will have a length of 6.

It is also worth mentioning that the `length()` method applies to all data types that are considered a subclass of the `CharSequence` interface, such as `String`, `StringBuilder`, and `StringBuffer`.

## See Also

- [Oracle Documentation - String length() Method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [GeeksforGeeks - Java String length() Method](https://www.geeksforgeeks.org/java-string-length-method-example/)
- [JavaTpoint - Java String length() Method](https://www.javatpoint.com/java-string-length)