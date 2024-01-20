---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Find the Length of a String in Java

## What & Why?
Finding the length of a string in Java boils down to counting the number of characters in a given string. Programmers often resort to this to control an iteration, validate data, or simply manipulate strings more precisely. 

## How To:

The `length()` function in Java gets the Job done. Let's illustrate with an example:

```Java
public class Main {
    public static void main(String[] args) {
        String str = "Hello, programmers!";
        System.out.println("Length of the string is " + str.length());
    }
}
```
When you run this, you'll get:

```Java
Length of the string is 19
```
Voila! The string length is calculated and displayed efficiently!

## Deep Dive

The `length()` function has been a part of Java since its early versions. It plays a key role in the core of string manipulation, along with other crucial functions such as `charAt()`, `substring()`, among others.

While `length()` is the go-to method, there are alternative ways to find the length of a string in Java.

```Java
String str = "Hello, programmers!";
System.out.println(str.toCharArray().length);
```
This converts the string into a char array and grabs the length of the array.

The string length functionality is implemented in Java String API using the attribute `value.length`. Here, `value` is an array storing the characters of the string. So, when `length()` is invoked, it returns the length of the `value` array.

## See Also

For a deeper understanding, feel free to explore;
1. [Java String length() Method](https://www.w3schools.com/java/ref_string_length.asp)
2. [Java - Strings](https://www.tutorialspoint.com/java/java_strings.htm) 
3. [Length vs Length() in Java](https://www.geeksforgeeks.org/length-vs-length-java/)