---
title:    "C# recipe: Extracting substrings"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why Extract Substrings?

As a programmer, you may encounter situations where you need to manipulate strings in a certain way. One common task is extracting substrings, or smaller parts of a larger string. This can be useful in many scenarios, such as parsing user input or formatting data for output. Knowing how to extract substrings can make your code more efficient and improve the overall functionality of your program.

## How To Extract Substrings in C#

To extract a substring in C#, you can use the `Substring()` method. This method takes in two parameters - the starting index and the length of the substring you want to extract. Here's an example of how you can use this method:

```
string sentence = "I love programming!";
string substring = sentence.Substring(2,4);
Console.WriteLine(substring);
```

In this example, we are extracting the substring starting at index 2 (the third character) and with a length of 4 characters. The output would be "love", as it contains the four characters starting from index 2.

You can also use the `Substring()` method without specifying a length, which will extract the remaining characters from the specified index to the end of the string. For example:

```
string sentence = "I love programming!";
string substring = sentence.Substring(7);
Console.WriteLine(substring);
```

The output would be "programming!".

## Deep Dive into Substring Extraction

It's important to note that the `Substring()` method does not modify the original string, but instead returns a new string. This means that if you want to alter the original string, you need to assign the substring to a new variable.

Another useful method for extracting substrings is `Split()`. This method takes in a delimiter and returns an array of strings separated by that delimiter. For example:

```
string fruits = "apple, banana, orange";
string[] fruitList = fruits.Split(',');
Console.WriteLine(fruitList[0]); //Output: "apple"
```

In this example, we used a comma as the delimiter to split the string into an array of fruits.

## See Also

To continue learning about manipulating strings in C#, check out these helpful resources:

- [Microsoft Docs - Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [Codecademy - String Manipulation in C#](https://www.codecademy.com/learn/learn-c-sharp/modules/csh-string-manipulation)
- [C# Corner - Understanding Substring Method in C#](https://www.c-sharpcorner.com/article/understanding-substring-method-in-c-sharp/)