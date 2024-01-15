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

## Varför
Att läsa och behandla textfiler är en vanlig uppgift inom programmering, särskilt inom C#. Genom att kunna läsa en textfil kan du använda dess data för att utföra olika åtgärder, t.ex. skapa rapporter eller uppdatera databaser.

## Hur man gör det
För att läsa en textfil i C# behöver du först skapa en instans av klassen `StreamReader`. Sen använder du `File.OpenText()`-metoden för att öppna och läsa filen. Här är ett enkelt exempel:

```C#
StreamReader läsare = File.OpenText(@"C:\MinaFiler\exempeltextfil.txt");
while (!läsare.EndOfStream)
{
    string rad = läsare.ReadLine();
    Console.WriteLine(rad);
}
```

Det här kodblocket skapar en instans av `StreamReader` som heter `läsare` och använder sedan `while`-loopen för att läsa varje rad i textfilen tills den når slutet av filen. Varje rad sparas i variabeln `rad` och skrivs sedan ut i konsolfönstret.

För att läsa och behandla specifika delar av textfilen, som t.ex. endast vissa rader eller delar av en rad, kan du använda andra metoder som `Read()` och `ReadLine()`. Genom att använda dessa metoder i kombination med olika strängmanipulationsfunktioner, som `Substring()` och `Split()`, kan du extrahera precis den information du behöver från textfilen.

## Djupdykning
När du läser en textfil i C# behöver du hålla vissa saker i åtanke. Till exempel är det viktigt att stänga `StreamReader`-instansen när du är klar med den för att undvika att lämna filen öppen och orsaka problem med andra program som försöker få åtkomst till filen.

En annan utmaning kan vara att hantera teckenkodning när du läser en textfil. Om filen innehåller tecken från ett annat språk eller har ett särskilt teckenkodningsschema måste du ange rätt kodning när du skapar `StreamReader`-instansen. Annars kan du få in felaktig data från filen.

Det finns också vissa säkerhetsrisker med att läsa textfiler eftersom användare kan manipulera filen och injicera skadlig kod. Därför är det alltid viktigt att validera och bearbeta indata innan du använder den.

## Se även
Om du vill lära dig mer om att läsa och skriva till filer i C#, kan du titta på följande resurser:

- [Microsoft Dokumentation: How to read text from a file](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-read-text-from-a-file)
- [C# Corner: Reading and Writing Files in C#](https://www.c-sharpcorner.com/UploadFile/mahesh/reads-a-text-file-and-display-in-a-listbox-in-C-Sharp/)
- [Youtube: C# Tutorial for Beginners: Read Write Files (Text and CSV) in C#](https://www.youtube.com/watch?v=o4--Pq8gk8w)