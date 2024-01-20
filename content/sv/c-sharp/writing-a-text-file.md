---
title:                "Skriva en textfil"
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? 
Att skriva en textfil innebär att skapa och lagra data i en läsbar format. Programmerare gör detta för att spara användardata, loggar, konfigurationer eller för att underlätta dataöverföring mellan system.

## How to:
I C# använder vi klassen `StreamWriter` för att enkelt skriva text till en fil. Här är ett exempel:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"test.txt";

        // Använd 'using' för att säkerställa att resurserna frigörs korrekt efter användning
        using (StreamWriter writer = new StreamWriter(filePath))
        {
            writer.WriteLine("Hej Sverige!");
        }

        Console.WriteLine(File.ReadAllText(filePath));
    }
}
```
Kör du koden skapas `test.txt` med texten "Hej Sverige!" och output i konsolen blir textens innehåll.

## Deep Dive
Historiskt sett har textfiler varit ett av de huvudsakliga sätten att lagra data på grund av deras enkelhet. Alternativ innefattar XML, JSON, eller databaser, men textfiler är oftast lättast att använda för enkel datalagring och -hantering. När det gäller implementering, aktar man sig för teckenkodning och filåtkomstkonflikter, vilket `StreamWriter` hanterar smidigt.

## See Also
- Microsoft's dokumentation om `StreamWriter`: https://docs.microsoft.com/dotnet/api/system.io.streamwriter
- Mer om filåtkomst och säkerhet i .NET: https://docs.microsoft.com/dotnet/standard/io/file-access-permissions
- Introduktion till andra lagringsformat som JSON i C#: https://www.newtonsoft.com/json