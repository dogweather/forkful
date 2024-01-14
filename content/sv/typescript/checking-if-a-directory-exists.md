---
title:                "TypeScript: Kontrollera om en katalog finns"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en katalog finns kan vara en viktig del av din TypeScript-programmering. Genom att titta på om en katalog finns, kan du bestämma om du behöver skapa den eller hantera beroenden på rätt sätt.

## Så här gör du
Först och främst behöver du använda dig av `fs`-modulen för att arbeta med filsystemet i Node.js. För att kontrollera om en katalog finns använder vi oss av metoden `existSync()` och anger sökvägen till katalogen som ett argument. Här är ett exempel på funktionen för att kontrollera om en katalog med namnet "projekt" finns:

```TypeScript
import * as fs from 'fs';

function checkDirectory(directory: string): boolean {
  return fs.existsSync(directory);
}

console.log(checkDirectory('projekt')); // true
console.log(checkDirectory('falskkatalog')); // false
```

Funktionen returnerar `true` om katalogen finns och `false` om den inte finns. Du kan sedan använda detta resultat för att utföra lämpliga åtgärder, som att skapa katalogen om den inte finns.

## Fördjupning
Om du vill ha en mer detaljerad kontroll av katalogen kan du använda metoden `statSync()` från `fs`-modulen. Detta returnerar en instans av klassen `fs.Stats` som innehåller information om filen eller katalogen. Här är ett exempel på hur du kan använda detta:

```TypeScript
import * as fs from 'fs';

function checkDirectoryStats(directory: string): boolean {
  const stats = fs.statSync(directory);
  return stats.isDirectory();
}

console.log(checkDirectoryStats('projekt')); // true
console.log(checkDirectoryStats('index.js')); // false
```

Funktionen kontrollerar nu om sökvägen är en katalog genom `stats.isDirectory()` och returnerar `true` eller `false` beroende på resultatet.

## Se även
- [TypeScript-dokumentation: fs module](https://www.typescriptlang.org/docs/handbook/nodejs-modules.html#fs)
- [Node.js dokumentation: fs module](https://nodejs.org/api/fs.html)
- [fs-extra: En utökad version av fs-modulen](https://www.npmjs.com/package/fs-extra)