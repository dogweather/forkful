---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Kontrollera om en katalog finns. Det är så grundläggande som det låter: leta upp en mapp i ditt filsystem och se om den faktiskt finns. Varför gör programmerare detta? För att undvika felsituationer när de försöker läsa, skriva, eller manipulera filer i en mapp som kanske inte är där.

## Så här gör du:
För att kontrollera om en katalog finns, använder man med fördel inbyggda metoder i 'fs' modulen i Node.js. Här är ett enkelt exempel:

```Javascript
const fs = require('fs');

if (fs.existsSync('/sökväg/till/din/mapp')) {
  console.log("Mappen finns!");
} else {
  console.log("Mappen finns inte!");
}
```

När du kör detta skript, kommer det antingen att skriva ut "Mappen finns!" om mappen finns, eller "Mappen finns inte!" om den inte gör det. 

## Djup dykning
'-fs.existsSync' är en synkron version som kontrollerar om en mapp finns. Den har varit en del av Node.js sedan version 0.1.21, vilket är varför det är så brett använd i dagens Javascript kod. 

Alternativt kan du också använda '-fs.access()', som är en asynkron version. Du kan avgöra vilken att använda på din användarfall och prestandakrav. 

På implementeringsnivå utför dessa metoder faktiskt en systemanrop till filesystemet, vilket innebär att de kan vara relativt långsamma. I de flesta fall är detta inte ett problem, men om du behöver upprepa operationen många gånger kan det vara klokt att skapa en cache eller använda en annan teknik, som '-fs.watch()'.

## Se även
Det finns mycket mer att läsa om att arbeta med filer och kataloger i Node.js. Här är några nyttiga länkar för vidare läsning:

- Node.js dokumentation om 'fs' modulen: [fs - Node.js v16.6.1 Documentation](https://nodejs.org/dist/latest-v16.x/docs/api/fs.html)
- En bra StackOverflow-tråd om 'fs.access' vs 'fs.existsSync': [fs.existsSync vs fs.access - Stack Overflow](https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js)
- 'fs-extra' är ett mycket användbart npm-paket som utökar 'fs' med fler hjälpfunktioner: ['fs-extra' på npm](https://www.npmjs.com/package/fs-extra)