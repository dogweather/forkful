---
title:                "TypeScript: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kunna kontrollera om en mapp finns är en viktig del av programmering, särskilt när du arbetar med filhantering eller hanterar användarinput. Det är också en användbar funktion för att undvika fel och optimera ditt program. I denna bloggpost kommer vi att titta närmare på hur man kan kontrollera om en mapp finns i TypeScript.

## Hur man gör det

För att kontrollera om en mapp finns, använder vi ``fs.existsSync()`` funktionen från Node.js File System modulen. Den här funktionen tar en sökväg till mappen som argument och returnerar en boolean som visar om mappen finns eller inte. Om den hittar mappen returnerar den true, annars false.

Först behöver vi importera ``fs`` modulen i vårt TypeScript-program:

```TypeScript
import fs from 'fs';
```

Sedan kan vi använda ``fs.existsSync()`` funktionen för att kontrollera om en mapp med namnet "documents" finns i vår nuvarande sökväg:

```TypeScript
if (fs.existsSync('documents')) {
  console.log('Mappen finns.');
} else {
  console.log('Mappen finns inte.');
}
```

Om mappen existerar kommer utskriften att vara "Mappen finns.", annars kommer den visa "Mappen finns inte.".

## Djupdykning

En sak att notera är att ``fs.existsSync()`` funktionen endast returnerar true om mappen finns på exakt den sökväg som vi har angivit. Det betyder att om vi till exempel anger "documents/test" som sökväg istället för bara "documents", kommer funktionen att returnera false eftersom "documents/test" inte är en existerande mapp. För att kunna söka efter en mapp i en undermapp, måste vi använda ``fs.existsSync()`` funktionen med ``path.join()`` från Node.js Path modulen.

```TypeScript
import fs from 'fs';
import path from 'path';

const mappNamn = 'test';
const undermapp = 'documents';

const sökväg = path.join(undermapp, mappNamn);
if (fs.existsSync(sökväg)) {
  console.log('Mappen finns.');
} else {
  console.log('Mappen finns inte.');
}
```

Nu kommer utskriften att vara "Mappen finns." om "test" finns i undermappen "documents".

## Se även

- [Node.js File System modulen](https://nodejs.org/api/fs.html)
- [Node.js Path modulen](https://nodejs.org/api/path.html)