---
title:                "Läsa en textfil"
html_title:           "C#: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil är en vanlig uppgift för programmerare. Det innebär att man läser in data från en textbaserad fil och använder den i sitt program. Det kan vara användbart för att lagra och använda stora mängder information eller för att importera data från andra program.

## Hur man gör det:
Det finns flera sätt att läsa en textfil i C#, men ett enkelt sätt är att använda klassen `StreamReader`. Här är ett exempel på hur man läser in en textfil med detta sätt:

```C#
using var reader = new StreamReader("filnamn.txt");

// Läser in varje rad i filen och skriver ut den
string line;
while ((line = reader.ReadLine()) != null)
{
    Console.WriteLine(line);
}
```

Om filen innehåller följande text:

```
Detta är en textfil.
Här skriver vi lite information.
```

Så kommer koden ovan att skriva ut följande i konsolen:

```
Detta är en textfil.
Här skriver vi lite information.
```
 
## Djupdykning:
Att läsa en textfil är en viktig del av filhantering i programmering. Historiskt sett har filer använts för att lagra data på datorer, och att kunna läsa och bearbeta dessa filer är en viktig funktion för programmerare. Det finns olika alternativ för att läsa en textfil i C#, bland annat `File.ReadAllLines()` och `File.ReadAllText()`. Det är viktigt att komma ihåg att stänga en `Stream` efter att man har läst klart från den för att undvika minnesläckor.

## Se även:
Här är några länkar till relaterade källor som kan vara användbara för att lära sig mer om att läsa textfiler i C#:
- [Microsoft docs om StreamReader](https://docs.microsoft.com/sv-se/dotnet/api/system.io.streamreader?view=net-5.0)
- [En tutorial om C# filhantering](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)
- [En guide för att läsa och skriva till filer i C#](https://www.c-sharpcorner.com/article/file-handling-in-c-sharp/)