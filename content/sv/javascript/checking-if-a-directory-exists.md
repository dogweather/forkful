---
title:                "Javascript: Kontrollera om en mapp finns"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

När man programmerar är det viktigt att ha kontroll över filstrukturen i sitt projekt. Ibland kan det vara nödvändigt att kontrollera om en viss mapp eller katalog finns innan man fortsätter med sin kod.

## Så här gör du

För att kontrollera om en mapp finns i Javascript, kan du använda dig av den inbyggda funktionen `fs.existsSync()`. Detta kommer att returnera `true` om mappen finns och `false` om den inte finns. Nedan följer ett enkelt exempel på hur du kan använda denna funktion:

```Javascript
var fs = require('fs'); // Importera file system modulen

// Kontrollera om mappen "images" finns
if (fs.existsSync('images')) {
  console.log('Mappen finns!');
} else {
  console.log('Mappen finns inte.');
}
```

Om mappen "images" finns kommer output att vara "Mappen finns!". Om mappen inte finns kommer output att vara "Mappen finns inte.".

## Djupdykning

Det finns flera tillvägagångssätt för att kontrollera om en mapp finns i Javascript, men `fs.existsSync()` är den enklaste och mest effektiva lösningen. Det finns också andra metoder som `fs.stat()` och `fs.access()`, men dessa är mer avancerade och kräver mer kod.

Det är också viktigt att notera att `fs.existsSync()` endast fungerar för att kontrollera om en mapp finns, inte om en fil finns. För att kontrollera om en fil finns kan du använda `fs.existsSync()` i kombination med `fs.statSync()`, vilket ger dig mer detaljerad information om filen.

## Se även

- [Node.js File System Modulen](https://nodejs.org/api/fs.html)
- [Tutorial: Vad är mappar och filer?](https://www.digitalocean.com/community/tutorials/what-is-a-directory-file-system-structure-in-computer-programming)