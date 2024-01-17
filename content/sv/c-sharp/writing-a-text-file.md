---
title:                "Att skriva en textfil"
html_title:           "C#: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva en textfil i programmering betyder att man skapar en fil som innehåller text som kan läsas och bearbetas av datorn. Detta är en vanlig praxis bland programmerare eftersom det möjliggör för program att lagra och hämta data som används i programmet, som till exempel användarinställningar eller produktinformation.

## Hur man:
```C#
// Exempel på hur man skapar en textfil och skriver in texten "Hej världen!"
using System.IO;

// Skapa en ny textfil
FileStream file = new FileStream("minTextfil.txt", FileMode.Create);

// Skriv in texten i textfilen
StreamWriter writer = new StreamWriter(file);
writer.WriteLine("Hej världen!");

// Stäng textfilen
writer.Close();
file.Close();

// Resultatet blir en textfil med namnet "minTextfil.txt" som innehåller texten "Hej världen!"
```

## Djupdykning:
Att skapa textfiler har funnits sedan början av programmering och är en viktig del av datahantering. En annan vanlig metod för att lagra data är genom att använda en databas istället för en textfil. Detta gör det möjligt att lägga till fler funktioner som sökning och sortering av data. I C# finns också metoder för att hantera stora mängder data mer effektivt genom att använda "buffering" (hålla data i minnet för snabbare åtkomst).

## Se även:
- Microsoft C# dokumentation om filhantering: https://docs.microsoft.com/en-us/dotnet/standard/io/
- Så här skriver man en textfil i C#: https://www.c-sharpcorner.com/uploadfile/mahesh/how-to-write-to-file-in-C-Sharp/
- Skillnaden mellan textfiler och databaser: https://www.programmerinterview.com/data-structures/difference-between-a-text-file-and-a-database/