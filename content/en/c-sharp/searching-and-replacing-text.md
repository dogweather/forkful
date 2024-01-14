---
title:    "C# recipe: Searching and replacing text"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why

Have you ever found yourself manually replacing text in a long document or code file? It can be a tedious and time-consuming task. Thankfully, there is a solution - searching and replacing text using C# programming. This allows you to quickly make changes to multiple sections of text in one go, saving you time and hassle.

## How To

To search and replace text in C#, we will use the `Replace()` method. This method takes in three arguments - the original string, the string to be replaced, and the new string. Here's an example of how we can use this method to replace all instances of "Hello" with "Hi" in a sentence:

```C#
string sentence = "Hello there, Hello world!";
string newSentence = sentence.Replace("Hello", "Hi");
Console.WriteLine(newSentence);
```

The output of this code will be: "Hi there, Hi world!". As you can see, all instances of "Hello" have been replaced with "Hi".

We can also use this method to replace text in larger files. For example, let's say we have a text file with the following contents:

```
This is a sentence with the word apple.
Another sentence with apple.
```

If we want to replace "apple" with "orange" throughout the file, we can use the following code:

```C#
string fileContents = File.ReadAllText("data.txt"); //read file contents into a string
string newFileContents = fileContents.Replace("apple", "orange"); //replace "apple" with "orange"
File.WriteAllText("newData.txt", newFileContents); //write new file contents to a new file
```

The output of this code will be a new file with the following contents:

```
This is a sentence with the word orange.
Another sentence with orange.
```

## Deep Dive

The `Replace()` method is case-sensitive, which means that it will not replace instances of a string if the letter case does not match. For example, using the second code example above, if we wanted to replace "APPLE" with "orange", it would not work. To make the method case-insensitive, we can use the `StringComparison` enum and specify `IgnoreCase` as the comparison type.

```C#
string fileContents = File.ReadAllText("data.txt"); 
string newFileContents = fileContents.Replace("apple", "orange", StringComparison.OrdinalIgnoreCase); 
File.WriteAllText("newData.txt", newFileContents); 
```

This will now replace all instances of "apple" regardless of the letter case, resulting in the following output:

```
This is a sentence with the word orange.
Another sentence with orange.
```

## See Also

- [C# Replace method documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- [StringComparison enum documentation](https://docs.microsoft.com/en-us/dotnet/api/system.stringcomparison)
- [String.Replace vs Regex.Replace in C#](https://stackoverflow.com/questions/2674071/string-replace-vs-regex-replace-in-c-sharp)