---
title:    "Elm: Skapa en tillfällig fil"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att skapa temporära filer är en vanlig praxis inom programmering för att hantera data som behöver lagras temporärt eller delas mellan olika processer. Det kan också användas för att hålla koll på systemets status eller tillfälliga beräkningar.

## Så här gör du

För att skapa en temporär fil i Elm kan du använda funktionen `Temp.file`. Till exempel:

```Elm
Temp.file "min_fil" ".txt" >> Task.attempt handleResult
```

Funktionen tar två argument: namnet på filen och den önskade filtypen. Efter att funktionen har körts kommer `handleResult` att returnera en `Result` med antingen ett felmeddelande eller sökvägen till den skapade filen.

## Djupdykning

Om du behöver mer kontroll över skapandet av temporära filer kan du använda `Temp.fileWith` istället. Detta låter dig specificera det önskade formatet på filnamnet och även specificera var filen ska skapas. Exempelvis:

```Elm
Temp.fileWith
    { name = "tempfil"
    , path = Just "/tmp/"
    , extension = ".html"
    }
    >> Task.attempt handleResult
```

Genom att specificera en sökväg kan du välja var filen ska lagras, vilket kan vara användbart om du behöver ha kontroll över filerna som skapas.

## Se också

- [Elm-temp](https://package.elm-lang.org/packages/MichaelJPerry/elm-temp/latest/) - ett paket som underlättar hanteringen av temporära filer i Elm.
- [Temporära filer på Linux](https://www.linux.com/tutorials/working-temporary-files-linux/) - en djupare förklaring av hur temporära filer fungerar på Linux-system.