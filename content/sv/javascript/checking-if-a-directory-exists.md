---
title:    "Javascript: Att Kontrollera Om En Katalog Finns"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

Det är viktigt att ha en funktion som kan kontrollera om en mapp finns eller inte, eftersom detta kan hjälpa till att förhindra felaktig kodning och minska risken för buggar.

## Hur man gör

```Javascript
if (fs.existsSync(path)) {
  console.log('Mappen finns');
} else {
  console.log('Mappen finns inte');
}
```

Koden ovan använder den inbyggda Node.js-funktionen `fs.existsSync()` för att kontrollera om en mapp existerar. Funktionen tar in sökvägen till mappen som ett argument och returnerar `true` om mappen finns och `false` om den inte gör det. Sedan kan vi använda en `if`-sats för att utföra en handling beroende på resultatet.

I följande exempel ber vi användaren att ange en sökväg och sedan kontrollerar vi om den mappen existerar eller inte.

```Javascript
const fs = require('fs');
const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

rl.question('Ange sökvägen till mappen: ', (path) => {
  if (fs.existsSync(path)) {
    console.log('Mappen finns');
  } else {
    console.log('Mappen finns inte');
  }

  rl.close();
});
```

Om användaren anger en sökväg till en befintlig mapp kommer programmet att skriva ut "Mappen finns". Om användaren anger en sökväg till en mapp som inte existerar kommer programmet att skriva ut "Mappen finns inte".

## Djupdykning

Node.js tillhandahåller en mängd olika sätt att kontrollera om en mapp existerar. En annan metod är att använda `fs.stat()` som returnerar information om en fil eller mapp och ger en felkod om det inte går att hämta informationen.

```Javascript
fs.stat(path, (err, stats) => {
  if (err) {
    console.log(err);
  } else {
    // Om stats är undefined betyder det att mappen inte finns
    if (stats === undefined) {
      console.log('Mappen finns inte');
    } else {
      console.log('Mappen finns');
    }
  }
});
```

Det finns också paket som Fs-Extra som erbjuder mer avancerade funktioner för att kontrollera om en mapp existerar.

## Se även

- [Node.js fs modul](https://nodejs.org/api/fs.html)
- [Fs-Extra paket](https://www.npmjs.com/package/fs-extra)