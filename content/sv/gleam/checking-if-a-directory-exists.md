---
title:    "Gleam: Kontrollera om en mapp existerar"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp existerar är en vanlig uppgift för programmerare. Genom att kunna identifiera om en mapp finns eller inte kan man säkerställa att ens kod fungerar korrekt och undvika oväntade fel.

## Hur man gör det
Att kontrollera om en mapp existerar i Gleam är en relativt enkel process. Vi kan använda oss av den inbyggda standardmodulen `gleam/io/fs` för att få tillgång till funktionen `exists`, som tar in en sträng som representerar mappens sökväg. Om mappen finns returnerar funktionen `ok`, annars returneras `error` med en förklarande text.

```Gleam 
// Importera standardmodulen
import gleam/io/fs

// Definiera sökvägen till mappen vi vill kontrollera
let directory_path = "C:/Users/Username/Documents/Gleam_Project"

// Kontrollera om mappen existerar
fs.exists(directory_path)

// Exempel på output:
ok
```

## Djupdykning
Några saker att tänka på när man kontrollerar om en mapp existerar är sökvägens format och eventuella felmeddelanden som kan returneras. Ofta behöver sökvägen anges i ett specifikt format för att funktionen ska fungera korrekt, och felmeddelanden kan ge värdefull information för att felsöka eventuella problem.

En annan viktig aspekt att tänka på är att funktionen `exists` endast kontrollerar om en mapp existerar, inte om den är åtkomlig eller om man har rättigheter att läsa eller skriva till den. Detta kan vara av betydelse beroende på vad man ska använda informationen för.

## Se även
- [Dokumentation för Gleams standardmodul `gleam/io/fs`](https://gleam.run/docs/stdlib/fs/)
- [Guide till Gleams grundläggande syntax](https://gleam.run/docs/guide/)
- [Interaktiv Gleam-lärandemiljö](https://try.gleam.run/)

För mer information om Gleam och dess möjligheter, besök [Gleams officiella hemsida](https://gleam.run/).