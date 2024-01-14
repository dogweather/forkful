---
title:                "Javascript: Kontrollera om en katalog existerar"
simple_title:         "Kontrollera om en katalog existerar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av att utveckla en robust JavaScript-applikation. Genom att utföra denna kontroll kan du undvika eventuella buggar som kan uppstå när din kod försöker hitta en mapp som inte finns.

## Så här gör du

Det finns flera sätt att kontrollera om en mapp existerar i JavaScript, men det vanligaste sättet är att använda sig av `fs`-modulen. Här är ett exempel på hur du kan göra det:

```Javascript
const fs = require('fs');

// Ange sökvägen till mappen du vill kontrollera
const directoryPath = './mapp';

// Använd fs.stat() för att kontrollera om mappen existerar
fs.stat(directoryPath, (error, stats) => {
    if (error) {
        console.log("Mappen finns inte.");
    } else {
        console.log("Mappen existerar.");
    }
});
```

Om mappen existerar så kommer `fs.stat()` att returnera information om mappen, annars kommer den att kasta ett fel med meddelandet "ENOENT" (entity not found).

Det finns också andra metoder som kan användas för att kontrollera om en mapp existerar, såsom `fs.existsSync()` och `fs.access()`. Det är viktigt att notera att metoden `fs.existsSync()` är synkron, vilket innebär att den kan blockera din kod om den tar lång tid att utföra. Det är därför bäst att använda `fs.stat()` eller `fs.access()` som är asynkrona och inte blockerar din kod.

## Deep Dive

När du använder `fs.stat()` för att kontrollera om en mapp existerar kan du även få information om andra egenskaper hos mappen, som till exempel dess storlek och ändringsdatum. Detta kan vara användbart om du behöver mer detaljerad information om en mapp i din applikation.

Det är också värt att nämna att om du behöver utföra flera operationer på samma mapp, såsom att läsa eller skriva till filer i mappen, så kan du använda `fs.access()` istället för att kontrollera om mappen existerar varje gång.

Det finns även andra moduler som kan användas för att kontrollera om en mapp existerar, såsom `path` och `glob`. Dessa moduler erbjuder mer avancerade funktioner för att hantera filsystemet i JavaScript.

## Se även

- [Node.js - The fs Module](https://nodejs.org/docs/latest-v12.x/api/fs.html)
- [How to Check If a Directory Exists in Node.js](https://stackabuse.com/how-to-check-if-a-directory-exists-in-node-js/)
- [Node.js - The path Module](https://nodejs.org/docs/latest-v12.x/api/path.html)