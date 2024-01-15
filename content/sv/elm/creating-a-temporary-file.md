---
title:                "Skapa en temporär fil"
html_title:           "Elm: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en tillfällig fil är ett vanligt problem som många utvecklare står inför. Det kan handla om att behöva hantera temporära data eller skapa en mellanlagring för en process. Oavsett anledning kan Elm ge en enkel och effektiv lösning på detta problem.

## Så här gör du

För att skapa en temporär fil i Elm kan vi använda oss av funktionen `File.temp`. Den tar emot två argument, ett filnamn och en eventuell mapp där filen ska skapas. Om ingen mapp anges kommer filen att skapas i den tillfälliga mappen för applikationen.

```Elm
File.temp "tempFile.txt" Nothing
```

Detta kommer att skapa en temporär fil med namnet "tempFile.txt" i den tillfälliga mappen. Om vi vill skapa filen i en specifik mapp kan vi använda en `Just` med den önskade mappens sökväg som andra argument.

```Elm
File.temp "log.txt" (Just "/Users/username/logs")
```

Vi kan även specificera vilken typ av fil vi vill skapa genom att använda funktionen `File.tempWithType`. Den tar emot samma argument som `File.temp` och ett extra argument för filtypen.

```Elm
File.tempWithType "tempFile" "csv" Nothing
```

Detta kommer att skapa en temporär CSV-fil med namnet "tempFile" i den tillfälliga mappen.

## Djupdykning

När vi skapar en temporär fil i Elm används operativsystemets tillfälliga mappar för att lagra filen. Detta gör att filen kommer att raderas automatiskt när applikationen stängs ner. Om vi behöver spara filen permanent kan vi istället använda funktionen `File.write`.

```Elm
File.write "permFile.txt" "Text som ska sparas i filen"
```

Detta kommer att skapa en permanent fil med namnet "permFile.txt" i den nuvarande mappen och spara den angivna texten i filen. Vi kan även ändra filtyp genom att ange det som ett tredje argument.

```Elm
File.write "data" "JSON data" "json"
```

I denna kod kommer filen att sparas som en JSON-fil med namnet "data". Det är även möjligt att skriva till en specifik mapp genom att använda funktionen `File.writeTo`.

```Elm
File.writeTo "/Users/username/files" "doc.txt" "Text som ska sparas i filen" "txt"
```

Detta skapar en textfil med namnet "doc.txt" i mappen "/Users/username/files". Filen kommer fortfarande att sparas permanent, men i en specifik mapp istället för den tillfälliga mappen.

## Se även

- [Officiell Elm dokumentation för File](https://package.elm-lang.org/packages/elm/file/latest/)
- [Elm guide: File API](https://guide.elm-lang.org/interop/file.html)