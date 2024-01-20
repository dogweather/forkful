---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file in C# essentially means constructing a new, temporary storage file on your computer. Programmers often use these files to stash data that's too cumbersome or unnecessary to keep in memory.

## How to:

Here's some compact code to create a temporary file in C#:

```C#
string path = System.IO.Path.GetTempFileName();
```

That's it. You have a new temporary file. The `GetTempFileName` function automatically generates a unique name for your file, which you can access through the `path` variable. The resulting file gets parked in your system's temp folder.

Your new temporary file has a `.tmp` extension. To demonstrate, let's write some code to output the name of the randomly generated temporary file:

```C#
Console.WriteLine(path); // Outputs: C:\Users\YourUser\AppData\Local\Temp\tmpXXXX.tmp
```

Replace 'XXXX' with the specific random characters assigned to your file.

## Deep Dive:

The concept of temporary files isn't new; it's been around since the early days of computing. Files in the temp folder get cleared out from memory when you restart your system.

As for alternatives to `GetTempFileName()`, consider `Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())`. This code creates a file with a random name in your Temp folder, but unlike `GetTempFileName()`, it leaves the new file uncreated—allowing you the flexibility to decide what file type it should be. Here's what that looks like:

```C#
string path = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
Console.WriteLine(path); // Outputs: C:\Users\YourUser\AppData\Local\Temp\s1kxqkii.zgd (example)
```

The downside? You're responsible for cleaning these files up when you're done. The operating system won't dispose of them automatically.

When creating temporary files, you must mind safety. Use `Path.GetTempFileName()`, which operates securely under concurrent access. If two processes request temp files, they'll each get unique files.

## See Also:

For more info on `GetTempFileName()`, delve into the [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename). Dive deeper into temp files at [GeeksforGeeks](https://www.geeksforgeeks.org/c-sharp-path-gettempfilename-method/). Always keep an eye on how you handle your temporary files—this [StackOverflow thread](https://stackoverflow.com/questions/581570/how-can-i-create-a-temp-file-in-c-sharp) has wealth of cautionary tales.