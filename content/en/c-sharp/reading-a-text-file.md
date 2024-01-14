---
title:    "C# recipe: Reading a text file"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why

Text files are a fundamental part of programming and can contain important information for your code to function properly. Learning how to read a text file can greatly expand your coding capabilities and help you understand how to work with data in a more efficient way. In this blog post, we will explore the process of reading a text file in C# and how to use this skill in your own projects.

## How To

Reading a text file in C# is a relatively simple process. First, we need to create a variable that will hold the path to our text file. This can be done using the `string` data type, which is used to store text.

```
string filePath = "C:\\Users\\User\\Desktop\\myFile.txt";
```

Next, we need to use the `File` class from the `System.IO` namespace to actually read the contents of the file. We can do this by using the `ReadAllLines` method, which will read each line of the file and store it in an array.

```
string[] lines = File.ReadAllLines(filePath);
```

Now we can use a `foreach` loop to iterate through each line in the array and output it to the console.

```
foreach (string line in lines)
{
    Console.WriteLine(line);
}
```

If we have a text file with the following content:

```
Hello
World
```

The output of our code will be:

```
Hello
World
```

Congratulations! You have successfully read a text file using C#.

## Deep Dive

There are a few important things to note when reading a text file in C#. Firstly, the `ReadAllLines` method will throw an exception if the specified file does not exist or if it is unable to read the file. To avoid this, we can use the `File.Exists` method to check if the file exists before attempting to read it.

```
if (File.Exists(filePath))
{
    string[] lines = File.ReadAllLines(filePath);
}
else
{
    // file does not exist, handle error
}
```

Additionally, the `ReadAllLines` method will read the entire file into memory, which may cause issues if the file is very large. To avoid this, we can use the `StreamReader` class, which will read the file line by line, thus reducing memory usage.

```
using (StreamReader reader = new StreamReader(filePath))
{
    string line;
    while ((line = reader.ReadLine()) != null)
    {
        Console.WriteLine(line);
    }
}
```

## See Also

- [Microsoft Docs - How to: Read and Write to a Newly Created Data File](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-read-and-write-to-a-newly-created-data-file)
- [C# File Handling](https://www.tutorialspoint.com/csharp/csharp_file_handling.htm)
- [Working with Files in C#](https://www.c-sharpcorner.com/blogs/working-with-files-in-c-sharp)

Now that you have learned how to read a text file in C#, you can apply this knowledge to a variety of projects such as data processing, logging, and more. Keep exploring and honing your skills, as there are endless possibilities with text files in programming.

Happy coding!