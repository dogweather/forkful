---
title:                "Elm: Läsa en textfil"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa en textfil är en viktig färdighet för alla som lär sig programmera. Det låter dig läsa in data från en extern källa och använda den i ditt program. Om du vill lära dig hur man gör detta i Elm, är du på rätt plats!

## Hur man gör det

För att läsa en textfil i Elm, behöver du först importera den inbyggda modulen `File`:

```Elm
import File exposing (readTextFile)
```

Sedan kan du använda funktionen `readTextFile` för att läsa in en fil med hjälp av dess URL. Här är ett exempel som läser in en fil med namnet `textfile.txt`:

```Elm
readTextFile "textfile.txt"
```

Detta kommer att returnera en `Task String` som innehåller innehållet i filen som en sträng. Du kan sedan använda `Task`-modulen för att utföra den här uppgiften och få den faktiska strängen:

```Elm
Task.perform Debug.log >> Result.withDefault "" >> readTextFile "textfile.txt"
```

I detta exempel använder vi `Debug.log`-funktionen för att logga strängen till konsolen och `Result.withDefault` för att definiera en standardsträng om filen inte kan läsas in av någon anledning.

## Djupdykning

När du läser en textfil i Elm är det viktigt att förstå att detta är en asynkron operation. Det betyder att du behöver använda `Task`-modulen för att kunna hantera eventuella fel och få resultatet av läsningen.

En annan viktig sak att notera är att om du vill läsa in en fil från ett annat domän än din webbsida, behöver du använda ett CORS-proxy eller ändra dina inställningar för webbläsarsäkerhet.

## Se även

- [Elm dokumentation om att läsa filer](https://package.elm-lang.org/packages/elm/file/latest/)
- [CORS-proxy för elm-file](https://github.com/elm-explorations/file/blob/master/EXAMPLE.md#using-a-cors-proxy)
- [Elm-community lösning för CORS-proxy](https://github.com/elm-community/proxy)