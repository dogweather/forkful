---
title:                "Kontrollera om en mapp finns"
html_title:           "TypeScript: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När programmerare pratar om att kontrollera om en mapp existerar, så menar vi att vi vill veta om en fil finns på en viss sökväg. Det är användbart för att undvika felmeddelanden eller krascher i våra program, då vi kan ta olika åtgärder beroende på om mappen finns eller inte.

## Hur görs det:
Ett enkelt sätt att kontrollera om en mapp existerar i TypeScript är genom att använda inbyggda funktioner som finns i Node.js. Här är ett exempel på hur det kan se ut:

```TypeScript
import fs from 'fs';

fs.existsSync('./min-mapp'); // returnerar true om mappen finns
```

Om mappen inte finns blir resultatet "false". Detta kan sedan tas som ett villkor för att utföra olika åtgärder i koden.

## Deep Dive:
Historiskt sett så har kontrollen av mappar och filer varit viktig för att hålla ordning på filstrukturerna i operativsystem. I modern programmering så behöver vi fortfarande denna funktion för att säkerställa att vi inte försöker läsa en fil som inte existerar eller att vi försöker skriva till en mapp som inte finns.

En annan möjlig metod för att kontrollera mappar är genom att använda "try/catch" block i kombination med "stat" funktionen i Node.js. Detta ger oss mer flexibilitet i hur vi vill hantera eventuella felmeddelanden som uppstår om mappen inte existerar.

Det är också värt att notera att kontroll av mappar är något som bara behöver göras i miljöer där filstrukturer kan ändras, som till exempel på en webbserver.

## Se även:
- https://nodejs.org/api/fs.html#fs_fs_exists_path_callback - Node.js dokumentation för "fs.existsSync" funktionen.
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch - Dokumentation för "try/catch" block i JavaScript.