---
title:                "Läsa en textfil"
html_title:           "TypeScript: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en grundläggande uppgift inom programmering, oavsett vilket språk man använder. Det är viktigt att förstå hur man läser en textfil för att kunna hämta och använda data från olika källor, som till exempel en databas eller en webbserver.

## Hur man gör det

För att läsa en textfil i TypeScript använder man funktionen readFile() från File System-modulen. Detta gör man genom att först importera modulen som "fs" och sedan anropa readFile() med sökvägen till den önskade filen och en kodningssträng som argument.

```TypeScript
import fs from "fs";

fs.readFile("exempelfil.txt", "utf-8", (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

I exemplet ovan läser vi filen "exempelfil.txt" och loggar ut dess innehåll till konsolen. Notera att readFile() är en asynkron funktion och tar emot en callback-funktion som argument. Detta gör att man måste hantera eventuella fel som kan uppstå vid läsning av filen.

## Fördjupning

När man läser en textfil i TypeScript så skickas filens innehåll som en sträng tillbaka som resultat. För att kunna använda den datan i vår kod, kan vi behöva göra vissa manipulationer, som att dela upp strängen i mindre delar eller omvandla den till ett annat format som en array eller ett objekt.

Det finns också andra metoder för att läsa en textfil, som till exempel readFileSync() som returnerar filinnehållet som en sträng utan att behöva använda en callback-funktion. Det är också möjligt att ange andra sökvägar beroende på var filen befinner sig, som till exempel en relativ sökväg från vår projektmapp.

## Se även

- [Dokumentation för File System-modulen](https://nodejs.org/api/fs.html)
- [Läs en textfil i JavaScript](https://www.programiz.com/javascript/examples/read-file)
- [Manipulera textfiler i TypeScript](https://www.techiediaries.com/node-typescript-write-read-files/)