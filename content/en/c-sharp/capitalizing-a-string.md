---
title:                "C# recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to capitalize a string in your C# code? Maybe you're working on a web application and need to display a user's name in all capital letters or you're creating a console program and want to make the user's input all uppercase. Whatever the reason, knowing how to capitalize a string in C# can be a useful tool in your programming arsenal.

## How To

To capitalize a string in C#, you can use the built-in method `ToUpper()` which converts all lowercase characters in a string to uppercase. Let's take a look at an example:

```C#
string name = "john doe";
string capitalizedName = name.ToUpper();
Console.WriteLine(capitalizedName);
```

In this code, we first declare a string variable `name` with the value "john doe". Then, we use the `ToUpper()` method to convert the string to all uppercase letters and store the result in a new variable `capitalizedName`. Finally, we use the `Console.WriteLine()` method to output the capitalized name to the console.

The console will display "JOHN DOE" as the output. 

You can also use the `ToUpper()` method directly on a string without creating a new variable like this:

```C#
string name = "john doe";
Console.WriteLine(name.ToUpper());
```

The output will be the same as before, "JOHN DOE".

## Deep Dive

It's important to note that the `ToUpper()` method only converts lowercase characters to uppercase. Any characters that are already uppercase or non-alphabetical characters will remain unchanged. For example:

```C#
string sentence = "Today is a sunny day!";
Console.WriteLine(sentence.ToUpper());
```

The output will be "TODAY IS A SUNNY DAY!" because the "!" and the space between "a" and "sunny" are already non-alphabetical characters.

You can also use `ToUpper()` in combination with other string methods, like `Substring()` and `IndexOf()`, to manipulate specific parts of a string. For example:

```C#
string songLyrics = "I've been staring at the edge of the water";
int index = songLyrics.IndexOf("staring");
Console.WriteLine(songLyrics.Substring(0, index).ToUpper() + songLyrics.Substring(index));
```

In this code, we first use the `IndexOf()` method to find the index of the word "staring" in the string. Then, we use the `Substring()` method to split the string into two parts - the first part from the beginning of the string to the index of the word "staring" and the second part from the index to the end of the string. We then use `ToUpper()` on the first part to capitalize it and combine it with the second part using the `+` operator. The output will be "I'VE BEEN STARING at the edge of the water".

## See Also

To learn more about string methods in C#, check out these resources:

- [Microsoft Docs: String Methods](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0#methods)
- [C# String Functions with Examples](https://www.guru99.com/c-sharp-string.html)
- [C# String Manipulation](https://www.c-sharpcorner.com/blogs/string-manipulation-in-c-sharp-programming1)

Happy coding!