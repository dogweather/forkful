---
title:    "C# recipe: Reading a text file"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
As a programmer, you might come across a situation where you need to read data from a text file. This could be for importing data to your application, analyzing data, or simply displaying the contents of a file to a user. Whatever the reason may be, knowing how to read a text file using C# is a useful skill to have.

## How To
To read a text file using C#, you will need to follow these steps:

1. Create a `StreamReader` object by passing the path to the text file as a parameter.
2. Use the `ReadLine()` method to read each line of the text file.
3. Store the read lines into a variable to be used later in your code.
4. Close the `StreamReader` object once you are done reading the file.

Here is an example of how this would look in code, with a sample output:

```C#
// Create StreamReader object
StreamReader reader = new StreamReader("sample.txt");
// Read each line of the text file
string line;
while ((line = reader.ReadLine()) != null)
{
    // Store read lines into a variable
    string content = line;
    // Display content to console
    Console.WriteLine(content);
}
// Close StreamReader object
reader.Close();
```

Sample Output:
```
This is a sample text file.
It contains multiple lines of text.
We will be reading and displaying these lines using C# code.
```

## Deep Dive
There are a few important things to keep in mind when reading a text file using C#:

- Make sure to handle any errors that may occur during the process.
- Use the appropriate encoding when reading the file, especially if it contains non-ASCII characters.
- Consider using a `using` statement to automatically dispose of the `StreamReader` object once it is no longer needed.

Additionally, there are multiple methods available for reading text files in C#, such as `ReadToEnd()` or `ReadAllLines()`, which may be more suitable for certain situations. It is important to research and choose the appropriate method for your specific needs.

## See Also
For more information on reading text files in C#, check out these resources:

- [Microsoft Docs: How to Read and Write to a Newly Created Data File](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file)
- [C# Corner: Reading and Writing Text Files using C#](https://www.c-sharpcorner.com/article/reading-and-writing-text-file-using-C-Sharp/)
- [TutorialsTeacher: Read Text File in C#](https://www.tutorialsteacher.com/csharp/csharp-read-text-file)

By following these steps and considering the additional information provided, you should now be able to confidently read a text file using C# in your own projects. Happy coding!