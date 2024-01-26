---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:56:13.716704-07:00
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? / Що та Чому?
Перевірка існування директорії – це процес, що дозволяє визначити, чи фактично існує папка на файловій системі. Програмісти роблять це для уникнення помилок під час спроби доступу або модифікації файлів у неіснуючій директорії.

## How to: / Як це зробити:
```C#
using System;
using System.IO;

class Program {
    static void Main() {
        string path = @"C:\MyFolder";

        if (Directory.Exists(path)) {
            Console.WriteLine("Directory exists.");
        } else {
            Console.WriteLine("Directory does not exist.");
        }
    }
}
```
Sample Output:
```
Directory exists.
```
or
```
Directory does not exist.
```
Above code checks if `MyFolder` exists in the `C:` drive.

## Deep Dive / Поглиблений Розділ:
Checking if a directory exists in C# can be traced back to the .NET Framework days, using the `System.IO` namespace. Alternatives include creating a directory if it doesn't exist with `Directory.CreateDirectory(path)` or trying to access the directory and handling exceptions. The check is implemented using native Windows API calls or system calls on other operating systems, abstracted by .NET's runtime environment to work across platforms.

## See Also / Див. також:
- MSDN System.IO.Directory.Exists method: https://learn.microsoft.com/en-us/dotnet/api/system.io.directory.exists
- Microsoft's guide to file system IO: https://learn.microsoft.com/en-us/dotnet/standard/io/file-system
- Stack Overflow discussions on directory checking in C#: https://stackoverflow.com/search?q=C%23+check+if+directory+exists
