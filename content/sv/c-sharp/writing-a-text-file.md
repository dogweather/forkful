---
title:                "Skriva en textfil"
html_title:           "C#: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är ett sätt att lagra information på ett strukturerat och enkelt sätt. Det kan vara användbart för att skapa en backup av ditt program, spara användardata eller skriva ut rapporter.

## Såhär gör du

För att skriva en textfil i C# behöver du först skapa en instans av klassen ```StreamWriter```. Sedan kan du använda metoden ```WriteLine()``` för att skriva texten som du vill ska finnas i filen. När du är klar med att skriva måste du stänga filen med metoden ```Close()``` för att säkerställa att all data har sparats korrekt.

```C#
StreamWriter fil = new StreamWriter("minfil.txt"); //skapar en ny textfil
fil.WriteLine("Detta är en text som sparas i filen.");
fil.Close(); //stänger filen
```

Din textfil kommer nu att finnas sparad i samma mapp som ditt program.

Om du vill lägga till mer text i din textfil kan du använda metoden ```Append()```.

```C#
StreamWriter fil = new StreamWriter("minfil.txt", true); //true talar om att vi ska lägga till mer text
fil.WriteLine("Här kommer lite mer text.");
fil.Close(); //stänger filen
```

## Djupdykning

Förutom att skriva vanlig text i en textfil kan du även använda speciella tecken för att formatera texten. Till exempel kan du använda ```\t``` för att skapa ett tabbstopp, ```\n``` för en ny rad eller ```\r``` för att återgå till början av raden. Här är ett exempel som skapar en enkel tabell:

```C#
StreamWriter fil = new StreamWriter("minfil.txt");
fil.WriteLine("Förnamn\tEfternamn\tTelefonnummer"); //skapar tabellhuvud
fil.WriteLine("Pelle\tLarsson\t 072-123 45 67"); //skriver ut första raden
fil.WriteLine("Anna\tOlsson\t 073-234 56 78"); //skriver ut andra raden
fil.Close(); //stänger filen
```

Du kan också använda metoden ```Write()``` istället för ```WriteLine()``` för att skriva ut text på samma rad, utan ny rad-tecknet.

## Se även

- [Microsoft-dokumentation om klassen ```StreamWriter```](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter?view=net-5.0)
- [En guide till textfiler i C#](https://www.c-sharpcorner.com/UploadFile/mahesh/streamwriter-in-C-Sharp/)
- [YouTube-video som förklarar textfiler i C#](https://www.youtube.com/watch?v=bEB0xrqayjs)