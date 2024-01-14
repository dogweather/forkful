---
title:    "Java recipe: Extracting substrings"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Extracting substrings is a useful skill to have in Java programming as it allows you to manipulate strings in a more efficient and precise manner. This can be especially helpful when dealing with large amounts of text data or when you need to extract specific information from a string.

## How To

To extract a substring in Java, you can use the `substring()` method from the `String` class. This method takes in two parameters: the starting index and the ending index of the desired substring. Let's take a look at an example:

```Java
String sentence = "I love programming";
String substring = sentence.substring(7, 18);
System.out.println(substring); // Output: programming
```

In this example, we have a string `sentence` that contains the phrase "I love programming". We then use the `substring()` method to extract the word "programming" by specifying the starting index as 7, which is the index of the letter "p", and the ending index as 18, which is the index of the letter "g" plus one. This will return a new string containing the extracted substring, which we can then assign to a new variable `substring` and print out.

You can also use the `substring()` method to extract a substring from a specific index until the end of the string by omitting the second parameter. For example:

```Java
String sentence = "I love programming";
String substring = sentence.substring(2);
System.out.println(substring); // Output: love programming
```

This will return a new string starting from index 2, which is the letter "l", until the end of the original string.

## Deep Dive

The `substring()` method in Java is a powerful tool that allows you to manipulate strings in many different ways. Here are some other things to keep in mind when working with substrings:

- The starting index in the `substring()` method is inclusive, meaning that the character at that index will be included in the extracted substring. However, the ending index is exclusive, so the character at that index will not be included.

- You can use negative numbers as indices in the `substring()` method. This will count backwards from the end of the string, with -1 being the index of the last character.

- If you specify an invalid index in the `substring()` method, such as a negative number or an index that is larger than the length of the string, it will throw an `IndexOutOfBoundsException`.

- The `substring()` method returns a new string, leaving the original string unchanged. This is important to remember if you plan on further manipulating the original string.

## See Also

You can learn more about the `substring()` method and string manipulation in Java by checking out these resources:

- [Official Java String class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [GeeksforGeeks article on substring in Java](https://www.geeksforgeeks.org/java-string-substring-method-example/)
- [JavaTpoint tutorial on substring in Java](https://www.javatpoint.com/java-string-substring)