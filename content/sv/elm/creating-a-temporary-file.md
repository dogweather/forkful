---
title:                "Elm: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en tillfällig fil kan vara användbart i många situationer, speciellt inom programmering. Det kan till exempel vara användbart när man vill spara temporära data som bara behövs under en kort stund eller när man vill skapa en temporär backup av en befintlig fil.

## Hur man gör det

För att skapa en temporär fil i Elm, kan man använda funktionen `File.temp`. Detta skapar en fil med ett unikt namn som är garanterat att inte redan finnas på systemet. Här är ett exempel på hur man kan använda funktionen:

```elm
import File

tempFile : Task x File.Handle
tempFile =
  File.temp "miau.txt" -- skapar en temporär fil med namnet "miau.txt"

-- Den resulterande fil-handlingen kan användas för att skriva eller läsa data från filen.
```

När man anropar funktionen `File.temp` måste man ange en sträng som bestämmer namnet på filen. Funktionen returnerar sedan en `Task`, vilket är Elm's sätt att hantera asynkrona operationer. Man kan sedan använda `Task.perform` för att hantera resultatet, vilket, i detta fall, är en `File.Handle` som kan användas för att kommunicera med den temporära filen.

## Djupdykning

När man skapar en temporär fil i Elm, blir filen automatiskt borttagen när programmet avslutas. Detta kan dock vara problematiskt om man behöver spara data längre än så, eller om programmet kraschar innan filen har rensats upp. I dessa fall kan man använda funktionen `File.withTemp`, som ger en chans att städa upp filen själv genom att använda en funktion som anropas när filen inte längre behövs. Här är ett exempel på hur man kan göra det:

```elm
import File

writeToTempFile : File.Handle -> ()
writeToTempFile handle =
  -- gör något med fil-handlingen, till exempel skriv till filen

File.withTemp "miau.txt" writeToTempFile
-- filen är nu skapad och funktionen writeToTempFile kommer att anropas innan filen tas bort
```

Det finns också möjlighet att ange ett annat ställe där den temporära filen ska skapas, genom att använda funktionen `File.withTempIn`. Detta kan vara användbart om man till exempel vill spara en fil på en specifik plats på hårddisken istället för i den temporära katalogen.

## Se även

- [Elm dokumentation för `File`](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Elm kodexempel som demonstrerar temporära filer](https://github.com/elm/file/tree/master/example)