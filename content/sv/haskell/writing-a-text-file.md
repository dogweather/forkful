---
title:                "Haskell: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är en viktig aspekt av programmering, oavsett vilket programmeringsspråk man använder sig av. Genom att lära sig hur man skriver en textfil i Haskell kan man till exempel läsa och skriva data från en fil eller skapa en loggfil för ett program. Det finns många användningsområden för att kunna hantera textfiler, och det är därför en användbar färdighet att ha inom programmering.

## Så här gör du

För att kunna skriva en textfil i Haskell behöver du använda dig av funktionen `writeFile`. Denna funktion tar två argument: sökvägen till filen du vill skriva till och innehållet som ska skrivas. Ett enkelt exempel på hur man kan använda denna funktion är:

```Haskell
writeFile "hello.txt" "Hej världen!"
```
I detta exempel skriver vi en textfil med namnet "hello.txt" och innehållet "Hej världen!". Om du nu öppnar filen i en texteditor, kommer du se att den innehåller just den texten.

Man kan även använda sig av `appendFile` funktionen för att lägga till text till en fil som redan finns. Detta är användbart om du till exempel vill lägga till loggmeddelanden på slutet av en fil istället för att skriva om hela filen varje gång.

## Djupdykning

När man skriver en textfil i Haskell kan man oftast inte använda sig av åäö i filnamnen. Det finns dock en lösning på detta genom att använda funktionen `encodeString` från paketet `System.FilePath`. Denna funktion ersätter åäö med motsvarande engelska tecken, vilket gör att filnamnet kan användas utan problem.

För att kunna läsa från en textfil kan man använda sig av funktionen `readFile`, som tar filens sökväg som argument och returnerar en sträng med filens innehåll. Detta är användbart om du vill läsa data från en textfil till ditt program.

## Se även

- [Haskell Dokumentation](https://www.haskell.org/documentation/)
- [System.FilePath modulen](https://hackage.haskell.org/package/filepath-1.4.2.1/docs/System-FilePath.html)
- [Läs mer om textfiler i Haskell](https://wiki.haskell.org/File_handling)