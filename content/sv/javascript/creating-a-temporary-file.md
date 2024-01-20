---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skapa en tillfällig fil innebär att tillfälligt lagra data I en fil, som sedan raderas när den inte längre behövs. Programmerare gör detta för att undvika att överbelasta minnet, eller när dataintegritet behövs på en disk vid senare tillfället.

## Hur gör man:
Här är ett sätt att skapa en tillfällig fil i Javascript genom att använda Node.js `fs` modul:

```Javascript
var fs = require('fs');

fs.mkdtemp('/tmp/foo-', (err, folder) => {
  if (err) throw err;
  console.log(folder);
});
```

I skriptet ovan skapar `mkdtemp`-metoden en unik tillfällig mapp i `/tmp` mappen. ‘Foo-‘ prefixet ger oss en lätt identifierbar fil. Om det inte finns några fel, skriver mappen ut dess väg i konsolen.

## Deep Dive 
Historiskt sett, tillfälliga filer uppstod i unix-baserade system, där de vanligtvis lagras i `/tmp` eller '/var/tmp' mappar. JavaScript erbjuder `fs` modul genom Node.js för att arbeta direkt med filsystemet, som inkluderar skapandet av temporära filer.

Ett alternativ till `mkdtemp` vore att använda `tmp-promise` paket, som använder `promises` i stället för "callbacks", vilket gör att du kan använda `async / await` syntax.

När det gäller implementationsdetaljer, skapas en tillfällig fil eller mapp med unik namn genom att lägga till sex slumpmässiga tecken till prefixet som du har angett. Filerna raderas inte automatiskt, så det är viktigt att ta bort dem när de inte längre behövs.

## Se också
1. Node.js `fs` module docs: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
2. `tmp-promise` paket: [https://www.npmjs.com/package/tmp-promise](https://www.npmjs.com/package/tmp-promise)
3. Article on In-depth File System in Node.js: [https://www.sitepoint.com/understanding-module-exports-exports-node-js/](https://www.sitepoint.com/understanding-module-exports-exports-node-js/)