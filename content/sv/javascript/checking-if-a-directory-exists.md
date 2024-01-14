---
title:    "Javascript: Kontrollera om en katalog finns"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en katalog finns på din dator kan vara en viktig del av att skriva effektiv kod. Genom att kontrollera om en katalog existerar kan du till exempel undvika att skriva över viktig data eller skapa onödiga kataloger.

## Så här

För att kontrollera om en katalog finns på din dator kan du använda dig av följande kod i Javascript:

```Javascript
const fs = require('fs');
const path = 'Sökväg/till/din/katalog';

// Kontrollera om katalogen existerar
fs.access(path, (error) => {
    if (error) {
        console.log("Katalogen existerar inte");
    } else {
        console.log("Katalogen existerar");
    }
});
```

I det här  exempel används modulen "fs" för att läsa filesystemet och funktionen "access" för att kontrollera om den specifika sökvägen finns. Om katalogen inte existerar kommer en felmeddelande att visas, annars kommer ett meddelande som bekräftar att katalogen finns.

## Djupdykning

En vanligt användning av att kontrollera om en katalog finns är att undvika att råka skriva över viktig data. Till exempel om du skapar en mapp för att lagra användarens personliga dokument, kan du först kontrollera om katalogen redan existerar istället för att skriva över befintlig data. 

En annan användning är att skapa en ny katalog om den inte redan finns. Till exempel, om du vill lagra användarens bilder i en specifik mapp, kan du först kontrollera om katalogen finns och om inte, skapa den.

Att kontrollera om en katalog finns är också ett sätt att förbättra prestandan i din kod. Genom att undvika onödiga operationer, som att skapa en katalog som redan existerar, kan du spara tid och resurser. 

## Se även

- [Modulen "fs" i Node.js dokumentation](https://nodejs.org/api/fs.html)
- [Tutorial om "fs" modulen på W3Schools](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Användbar information om att jobba med filer och kataloger i Javascript](https://www.digitalocean.com/community/tutorials/how-to-use-the-node-js-file-system-module)