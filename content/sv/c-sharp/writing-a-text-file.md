---
title:                "C#: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att skriva en textfil är en grundläggande färdighet inom programmering. Det är ett sätt att lagra och organisera data på ett enkelt och läsbart sätt. Textfiler används ofta för att lagra konfigurationsinställningar eller annan viktig information för en applikation.

## Så här gör du
För att skriva en textfil i C# använder man sig av StreamWriter-klassen. Först skapar man ett StreamWriter-objekt som är kopplat till den fil man vill skriva till. Sedan kan man använda olika metoder för att skriva till filen.

```C#
// Skapa en ny textfil som heter "mittExempel.txt"
StreamWriter minFil = new StreamWriter("mittExempel.txt");

// Skriva en rad till filen
minFil.WriteLine("Detta är en rad i min textfil.");

// Stäng filen när man är klar
minFil.Close();
```

Efter att kodexemplet ovan har körts kommer det nu att finnas en fil som heter "mittExempel.txt" med texten "Detta är en rad i min textfil."

## Djupdykning
Det finns flera olika sätt att skriva till en textfil i C#, men StreamWriter är en av de enklare metoderna. Det finns också möjlighet att använda FileStream-klassen för mer avancerade scenarier eller att skapa en fil med en viss teckenkodning.

En annan viktig aspekt när man skriver en textfil är att tänka på att filen måste stängas när man är klar med den. Om man inte stänger filen kan det orsaka oönskade problem eller resurser som lämnas öppna.

## Se även
Nedan hittar du några länkar till mer information om hur man skriver en textfil i C#:

- [Microsoft Docs: Skriva till en textfil i C#](https://docs.microsoft.com/sv-se/dotnet/standard/io/how-to-write-text-to-a-file)
- [Tutorialspoint: C# - Skriva en fil](https://www.tutorialspoint.com/csharp/csharp_writing_files.htm)
- [C# Corner: Skriva till och läsa från en textfil i C#](https://www.c-sharpcorner.com/blogs/write-and-read-text-file-in-c-sharp1)
- [Stack Overflow: Hur man skapar en fil och skriver till den i C#](https://stackoverflow.com/questions/47715830/how-do-i-create-a-file-and-write-to-it-in-c-sharp)