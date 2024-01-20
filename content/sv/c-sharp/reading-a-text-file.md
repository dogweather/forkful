---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Läsa en textfil i C#: En översikt

## Vad & Varför?
Att läsa en textfil i programmering innebär att utvinna data från en textfil och presentera den på lämpligt sätt i ditt program. Programmers gör detta för att ladda, bearbeta, analysera eller överföra data.

## Hur man gör:

För att läsa en textfil kan du använda `StreamReader`-klassen i C#. Nedan är en enkel kodstruktur som visar hur det fungerar.

```C#
using System;
using System.IO;

public class ReadFile
{
    public static void Main()
    {
        using (StreamReader sr = new StreamReader("exempelfil.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

Koden ovan läser en textfil med namnet 'exempelfil.txt' rad för rad och skriver ut varje rad till konsolen.

## Djupdykning:
Att läsa en textfil är en grundläggande uppgift och var faktiskt en av de första funktionerna som programmeringsspråk stödde. Sedan dess har flera olika tekniker och verktyg utvecklats för att utföra denna uppgift.

Förutom `StreamReader`, kan C# programmerare även använda `File`-klassen för att läsa textfiler. Fördelen med att använda `File`-klassen är att det inte kräver explicit hantering av `Stream` objekten.

Det är värt att notera att för att läsa större filer, är det mer effektivt att använda `StreamReader` genom att det minskar minnesanvändningen genom att läsa filer i chunkar.

## Se även:
För mer detaljerad information och fler exempel på att läsa textfiler i C#, kan du besöka:
- [Dot Net Perls](https://www.dotnetperls.com/streamreader)