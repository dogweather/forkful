---
title:                "C#: Skapa en tillfällig fil"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Ibland kan det vara nödvändigt att skapa tillfälliga filer för att lagra data eller information som endast behövs temporärt. Det kan till exempel vara för att utföra en operation på en större fil utan att riskera att ändra den ursprungliga filen.

## Hur man gör det
Här är ett enkelt exempel på hur man kan skapa en temporär fil i C#:

```C#
using System;
using System.IO;

namespace TemporaryFile
{
    class Program
    {
        static void Main(string[] args)
        {
            // Skapa en unik filnamnssuffix baserat på aktuellt datum och tid
            string fileName = $"{Path.GetTempPath()}temporary_{DateTime.Now.ToString("MMddHHmmssffff")}.txt";

            // Skapa en temporär fil
            File.Create(fileName);

            // Skriv ut filnamnet på den skapade filen
            Console.WriteLine($"En temporär fil har skapats med filnamnet: {fileName}");
        }
    }
}
```

För att kunna använda oss av de nödvändiga klasserna för att hantera filer, måste vi inkludera namespace `System.IO`. Sedan definierar vi ett unikt filnamn baserat på aktuellt datum och tid, och skapar sedan en ny fil med hjälp av `File.Create()` metoden. Slutligen skriver vi ut filnamnet till konsolen för att bekräfta att en temporär fil har skapats.

Output:

```bash
En temporär fil har skapats med filnamnet: C:\Users\Anna\AppData\Local\Temp\temporary_0619242505794102.txt
```

## Djupdykning
Det finns flera olika metoder för att skapa och hantera temporära filer i C#. Ett alternativ till den enkla metoden som vi använde i det föregående exemplet är att använda klassen `Path` tillsammans med metoden `GetTempFileName()`, vilket skapar en fil i den temporära katalogen med ett unikt namn och returnerar hela sökvägen till filen.

En annan viktig aspekt att ta hänsyn till när man arbetar med temporära filer är att se till att de raderas efter att de inte längre behövs. Det finns flera olika sätt att göra detta, till exempel genom att använda metoden `File.Delete()` eller `File.Delete(fileName)` för att explicit ta bort filen.

## Se även
- [Microsoft Docs: File Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [Microsoft Docs: Path Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netcore-3.1)
- [Temorary Files in .NET: Good Practice, Bad Practice, and the Ultimate Disposal Patten](https://exceptionnotfound.net/temporary-files-in-net-good-practice-bad-practice-and-the-ultimate-disposal-pattern/)