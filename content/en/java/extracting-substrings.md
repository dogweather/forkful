---
title:                "Extracting substrings"
html_title:           "Java recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why: Improve String Manipulation Efficiency with Substrings

As a Java programmer, you may often find yourself working with strings, whether it's for data validation, parsing, or concatenation. Sometimes, you may need to work with only a small portion of a string, and that's where substring extraction comes in handy. By learning how to extract substrings, you can improve your string manipulation efficiency and make your code more concise and readable.

## How To: Extract Substrings in Java
To extract a substring in Java, you will need to use the `substring()` method from the `String` class. This method takes in two parameters: the starting index and the ending index of the substring you want to extract. Keep in mind that the starting index is inclusive, while the ending index is exclusive.

To better understand how the `substring()` method works, let's look at some examples:

````Java
String sentence = "I love Java programming.";
String substring1 = sentence.substring(2, 6); // Output: love
String substring2 = sentence.substring(7, 11); // Output: Java
````

In the first example, we start at index 2, which is the letter "l", and end at index 6, which is the letter "e". This extracts the word "love" from the original sentence. In the second example, we start at index 7, which is the letter "J", and end at index 11, which is the letter "a". This extracts the word "Java" from the sentence.

You can also use negative indices to count from the end of the string. For example, `sentence.substring(3, -3)` will start at the 3rd to last character and end 3 characters before the end of the string. In this case, the output will be "love Java programming".

## Deep Dive: Important Notes on Substring Extraction 
There are a few important things to keep in mind when working with substring extraction in Java. 
- The starting and ending indices must be within the bounds of the string. Otherwise, an `IndexOutOfBoundsException` will be thrown.
- The starting index cannot be greater than the ending index. 
- If the starting index is equal to the ending index, an empty string will be returned.
- It's important to be careful when using negative indices, as they can easily lead to unexpected results or errors.

## See Also
Here are some additional resources for working with substrings in Java:
- [Java documentation on the `substring()` method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [GeeksforGeeks tutorial on substring manipulation in Java](https://www.geeksforgeeks.org/java-string-substring/)