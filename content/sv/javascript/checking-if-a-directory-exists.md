---
title:                "Kontrollera om en mapp finns"
html_title:           "Javascript: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av att skriva kod i Javascript. Det kan hjälpa till att undvika fel och säkerställa att all nödvändig data finns tillgänglig innan man fortsätter med resten av koden.

## Hur man gör det

För att kontrollera om en mapp existerar i Javascript kan du använda `fs.existsSync()`-funktionen från Node.js-filhanteringspaketet. Det här är enklast att förstå genom ett exempel:

```Javascript
const fs = require('fs');

// Definiera namnet på den mapp du vill kontrollera
const mappNamn = "minMapp";

// Använd fs.existsSync() för att kontrollera om mappen finns
if (fs.existsSync(mappNamn)) {
  console.log("Mappen finns!");
} else {
  console.log("Mappen existerar inte.");
}
```

I det här exemplet kontrollerar vi om mappen "minMapp" existerar i den nuvarande sökvägen. Om den existerar skrivs "Mappen finns!" ut i konsolen, annars skrivs "Mappen existerar inte." ut.

## Djupdykning

`fs.existsSync()`-funktionen returnerar en boolean (true eller false) beroende på om mappen existerar eller inte. Om du vill hantera existerande och icke-existerande mappar på olika sätt kan du använda `fs.existsSync()` tillsammans med en if-sats för att utföra olika åtgärder baserat på resultatet.

En annan viktig aspekt att tänka på är den filväg som används för att kontrollera mappen. Om du inte väljer en specifik filväg kommer `fs.existsSync()` att söka efter mappen i den nuvarande arbetsmappen. Du kan ange en absolut eller relativ filväg för att kontrollera en specifik mapp.

## Se också

- [Node.js dokumentation om fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Enkel guide: Javascript filhantering med Node.js](https://www.digitalocean.com/community/tutorials/how-to-handle-the-file-system-in-node-js)