---
title:    "Java recipe: Finding the length of a string"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the length of a string in your Java program? Maybe you needed to validate user input or manipulate a string in a specific way. Either way, knowing how to find the length of a string is a basic but essential skill in Java programming.

## How To

To find the length of a string in Java, you can use the `length()` method of the `String` class. This method returns an integer representing the number of characters in the string.

```Java
//create a string variable
String myString = "This is a string!"

//use the length() method to find the length of the string
int length = myString.length();

//print the result
System.out.println("The length of the string is " + length);
```

The output of this code would be: `The length of the string is 17`, since there are 17 characters in the string "This is a string!".

You can also use the `length()` method on an empty string, which will return a length of 0.

```Java
//create an empty string
String emptyString = "";

//use the length() method
int length = emptyString.length();

//print the result
System.out.println("The length of the string is " + length);
```

The output of this code would be: `The length of the string is 0`.

## Deep Dive

It's important to note that the `length()` method counts the number of characters in a string, not the number of words. So if you have a string with multiple words separated by spaces, the `length()` method will count the spaces as characters as well.

```Java
//create a string with multiple words
String multipleWords = "This string has multiple words";

//use the length() method
int length = multipleWords.length();

//print the result
System.out.println("The length of the string is " + length);
```

The output of this code would be: `The length of the string is 29`, even though there are only 5 words in the string.

Additionally, the `length()` method counts special characters as well. For example, if your string has an apostrophe or a punctuation mark, it will be included in the length count.

```Java
//create a string with special characters
String specialChars = "Hey, it's a string with special characters!";

//use the length() method
int length = specialChars.length();

//print the result
System.out.println("The length of the string is " + length);
```

The output of this code would be: `The length of the string is 38`.

## See Also

- [Oracle Java String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [String length() method documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)