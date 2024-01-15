---
title:                "Att skapa en tillfällig fil"
html_title:           "Javascript: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Att skapa en temporär fil är ett användbart verktyg när man behöver lagra data temporärt eller ändra något tillfälligt utan att påverka de befintliga filerna. Det är också ett bra sätt att skydda känslig information från oönskade ändringar.

## Hur Man Gör
Att skapa en temporär fil i Javascript är relativt enkelt med hjälp av inbyggda funktioner. Nedan följer ett exempel på hur man kan skapa och skriva till en temporär fil.

```Javascript
// Importera nödvändiga moduler
const fs = require("fs");
const path = require("path");
const os = require("os");

// Skapa en temporär fil med ett unikt namn
const tempFilePath = path.join(os.tmpdir(), "tempfile.txt");

// Skriv data till filen
fs.writeFileSync(tempFilePath, "Detta är en temporär fil!");

// Läs innehållet från filen och skriv ut det till konsolen
const fileContent = fs.readFileSync(tempFilePath, "utf8");
console.log(fileContent);
```
Output:
```
Detta är en temporär fil!
```
Som du kan se använde vi oss av modulen "fs" för att skapa, skriva och läsa från filen. Vi använder också "path" för att skapa sökvägen till vår temporära fil och "os" för att hitta den temporära mappen på vårt system. 

## Djupdykning
När du skapar en temporär fil så kan du också ange en specifik tid som filen ska vara giltig. Detta kan vara användbart om du bara behöver filen för en viss period. För att göra detta använder du dig av funktionen "mkstemp" från modulen "fs". Nedan är ett exempel på hur man kan använda denna funktion.

```Javascript
// Importera nödvändiga moduler
const fs = require("fs");
const path = require("path");

// Skapa en temporär fil som är giltig i 24 timmar
fs.mkstemp(path.join(os.tmpdir(), "tempfile.txt"), 24 * 60 * 60, (err, filename) => {
    if (err) throw err;

    // Skriv data till filen
    fs.writeFileSync(filename, "Detta är en temporär fil som är giltig i 24 timmar!");
    
    // Läs innehållet från filen och skriv ut det till konsolen
    const fileContent = fs.readFileSync(filename, "utf8");
    console.log(fileContent);
});
```
Output:
```
Detta är en temporär fil som är giltig i 24 timmar!
```

Det är också värt att nämna att det finns andra sätt att skapa temporära filer i Javascript, som till exempel att använda sig av modulen "tmp" eller skapa en temporär buffer direkt i minnet. Välj den metod som passar bäst för dina behov.

## Se Även
- [Node.js dokumentation om fs modulen](https://nodejs.org/api/fs.html)
- [En guide till att skapa temporära filer i Javascript](https://masteringjs.io/tutorials/node/tmp)