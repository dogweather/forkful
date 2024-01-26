---
title:                "Kontrollera om en katalog finns"
html_title:           "Arduino: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns innebär att vi kollar om en specifik mapp finns på disken. Programmerare gör detta för att undvika fel, såsom `DirectoryNotFoundException`, och för att kunna skapa eller använda mappen om den faktiskt finns.

## Hur gör man:
```C#
using System;
using System.IO;

class DirectoryCheck
{
    static void Main()
    {
        string path = @"C:\min_katalog";

        // Kontrollera om katalogen finns
        if (Directory.Exists(path))
        {
            Console.WriteLine("Katalogen finns!");
        }
        else
        {
            Console.WriteLine("Katalogen finns inte.");
        }
    }
}
```

Sample Output:
```
Katalogen finns inte.
```

Om katalogen finns, byt uttrycket "Katalogen finns inte." med "Katalogen finns!".

## Fördjupning
Att kontrollera katalogers existens är grundläggande i filsystemshanteringen och har varit en del av programmering sedan de tidiga dagarna av persondatorer. I äldre system var sådana kontroller till och med mer kritiska på grund av begränsade användargränssnitt och feedback-mekanismer.

Alternativ till `Directory.Exists` kan inkludera att fånga undantag som `DirectoryNotFoundException` när man försöker accessera en katalog, men detta betraktas generellt som en sämre praxis eftersom det är dyrare prestandamässigt.

Implementeringsdetaljer i .NET Framework och .NET Core använder systemanrop för att avgöra filsystemsstatus. Detta betyder att `Directory.Exists` faktiskt ber systemet om informationen, vilket kan variera beroende på operativsystemet och dess filsystem.

## Se även
- Microsofts dokumentation för `Directory.Exists`: https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists
- Artikel om hantering av filer och kataloger i .NET: https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-enumerate-directories-and-files
