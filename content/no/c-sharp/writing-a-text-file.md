---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
simple_title:         "Skriving av en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Skriving til tekstfiler lar oss lagre data. Programmører gjør dette for dataoppbevaring, logging, eller datautveksling.

## How to:
```C#
using System;
using System.IO;

class WriteTextFile
{
    static void Main()
    {
        string path = "eksempel.txt";
        string text = "Hei! Dette er tekst i en fil.";

        // Skriv til filen
        File.WriteAllText(path, text);

        // Legge til tekst i eksisterende fil
        string additionalText = "\nHer legger vi til mer tekst.";
        File.AppendAllText(path, additionalText);

        // Lese og vise innholdet
        string readText = File.ReadAllText(path);
        Console.WriteLine(readText);
    }
}
```
Output:
```
Hei! Dette er tekst i en fil.
Her legger vi til mer tekst.
```

## Deep Dive
Tidligere brukte vi FileStream og StreamWriter for filskriving i .NET. `File.WriteAllText` og `File.AppendAllText` er høyere abstraksjoner som er enklere å bruke. `System.IO` namespace i C# inkluderer klasser for filhåndtering, noe som bidrar til sikker og effektiv datahåndtering.

## See Also
- [File.WriteAllText Method - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writealltext)
- [File and Stream I/O - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- [File.AppendAllText Method - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.appendalltext)
