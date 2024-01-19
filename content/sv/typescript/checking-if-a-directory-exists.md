---
title:                "Kontrollera om en katalog finns"
html_title:           "C: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns är en grundläggande men viktig uppgift i programmering. Vi gör detta för att säkerställa att vi inte hanterar oexisterande kataloger, vilket kan leda till fel och undantagsproblem i vår kod.

## Hur man gör:
Här är ett exempel på hur du kan kontrollera om en katalog finns med fs-modulen i Node.js.

```TypeScript
import * as fs from 'fs';

function directoryExists(path: string): boolean {
  try {
    return fs.statSync(path).isDirectory();
  } catch {
    return false;
  }
}

console.log(directoryExists('/path/to/directory')); // Exempel på utdatan: true eller false
```
I koden ovan försöker vi hämta statusinformation för en given sökväg med fs.statSync. Om sökvägen representerar en giltig katalog, returrerar vi 'true'; annars returrerar vi 'false'.

## Djupdykning
Användandet av fs-modulet för denna uppgift har sina rötter i Node.js ursprungliga utformning, där fs-modulen ingår som en grundläggande del i File System API. Även om fs-modulen är kraftfull och kompetent, finns det alternativ. Till exempel finns fs-extra-modulen, som inkluderar promise-baserade versioner av fs-operationerna, plus några användbara tillägg.

Ytterligare en implementeringsdetalj att notera är att vi använder den synkrona versionen av stat-operationen, fs.statSync, istället för den asynkrona fs.stat. Detta är för att förenkla koden i denna demonstration, men i ett verkligt scenario kan du vilja använda den asynkrona versionen för att undvika att blockera Node.js händelse loop.

## Se också
1. [Node.js fs documentation](https://nodejs.org/api/fs.html)
2. [fs-extra module on npm](https://www.npmjs.com/package/fs-extra)

Kom ihåg, var noga med att hantera dina filvägar och kataloger säkert för att undvika eventuella fel som kan störa din applikation!