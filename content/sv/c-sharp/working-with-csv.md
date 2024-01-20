---
title:                "Arbeta med csv"
html_title:           "C#: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV står för "Comma Separated Values" och det är ett vanligt format för att lagra och överföra tabellformaterade data. Det är ett strukturerat sätt att organisera data i rader och kolumner, med varje cell separerad av ett komma. Programmers väljer att arbeta med CSV eftersom det är lätt att förstå och ett vanligt sätt att utbyta data.

## Hur man: 
Att arbeta med CSV i C# är enkelt med hjälp av .NET's built-in CSV parser. Här är en enkel kod som visar hur man kan läsa och skriva CSV filer:

```
using System;
using System.IO;
using System.Linq;
using Microsoft.VisualBasic.FileIO;

namespace CSVExample
{
  class Program
  {
    static void Main(string[] args)
    {
      // Läs CSV fil
      using (var reader = new TextFieldParser("example.csv"))
      {
        // Ange kommatecken som separator
        reader.TextFieldType = FieldType.Delimited;
        reader.SetDelimiters(",");

        // Loopa igenom varje rad i filen
        while (!reader.EndOfData)
        {
          // Hämta alla celler i den aktuella raden som en lista
          var cells = reader.ReadFields();
          foreach (var cell in cells)
          {
            // Skriv ut värdet av varje cell
            Console.WriteLine(cell);
          }
        }
      }

      // Skapa en ny CSV fil
      using (var writer = new StreamWriter("new_example.csv"))
      {
        // Skriv data till filen
        writer.WriteLine("1, red, apple");
        writer.WriteLine("2, blue,berry");
        writer.WriteLine("3, green, lime");
      }
    }
  }
}
```

Exempel på utskrift:

```
1
red
apple
2
blue
berry
3
green
lime
```

## Djupdykning: 
CSV har funnits sedan 1970-talet och användes ursprungligen som en enkel och lättåtkomlig sätt att lagra och utbyta data mellan olika program. Men det finns också andra format som kan användas för tabellformaterade data, som JSON eller XML. När du arbetar med CSV data måste du också vara medveten om eventuella problem med data kodning och tolkning av kolonner med specialtecken.

En alternativ metod för att läsa och skriva CSV filer i C# är att använda en tredjeparts bibliotek, som CsvHelper eller FileHelpers. Dessa bibliotek erbjuder mer avancerade funktioner och möjligheten att automatiskt mappa CSV data till objekt i C#.

Om du behöver behandla stora CSV filer kan du också tänka på att använda strömning istället för att läsa hela filen på en gång. Detta kan förbättra prestandan i ditt program.

## Se även: 
- [CsvHelper](https://joshclose.github.io/CsvHelper/)
- [FileHelpers](https://www.filehelpers.net/)