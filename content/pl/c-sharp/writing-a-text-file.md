---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"

category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Zapisywanie pliku tekstowego to zapisanie danych w postaci czytelnej dla człowieka. Programiści robią to, by przechowywać konfiguracje, logi, lub wymieniać informacje między programami.

## How to:
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example.txt";
        string content = "Cześć! To jest przykładowy tekst.";

        File.WriteAllText(filePath, content);
        
        Console.WriteLine("Plik został zapisany.");
    }
}
```
Output:
```
Plik został zapisany.
```

## Deep Dive
Pisanie do pliku tekstowego w C# sięga czasów .NET Framework. Wybraną alternatywą do `File.WriteAllText` jest `StreamWriter`, który oferuje większą kontrolę, jak np. buforowanie danych. W szczegółach implementacyjnych, ważne jest odpowiednie zarządzanie zasobami, zwłaszcza przy dużych plikach, używając `using` lub zamykając strumień ręcznie.

## See Also
- Microsoft Docs o `File.WriteAllText`: https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writealltext
- Przewodnik Microsoft dotyczący strumieni danych w C#: https://docs.microsoft.com/en-us/dotnet/standard/io/
- Tutorial dotyczący obsługi plików w C#: https://www.c-sharpcorner.com/article/file-handling-in-C-Sharp/
