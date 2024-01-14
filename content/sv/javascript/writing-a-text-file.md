---
title:                "Javascript: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är en viktig del av programmering. Genom att skriva en textfil kan du spara information och använda den i dina program. Detta gör det också lättare att organisera och ändra kod vid behov.

## Hur man gör det

För att skriva en textfil i Javascript behöver du använda en inbyggd funktion som heter "fs". Här är ett enkelt exempel på hur du skapar en textfil med hjälp av fs:

```Javascript
// Importera fs-modulen
var fs = require('fs');

// Skapa en textfil
fs.writeFile('mittTextdokument.txt', 'Det här är min första textfil', function (err) {
  if (err) throw err;
  console.log('Textfilen har skapats!');
});

```

I detta exempel använder vi writeFile-funktionen för att skapa en textfil med namnet "mittTextdokument.txt" och lägger till texten "Det här är min första textfil". Om funktionen lyckas skrive skriver den ut meddelandet "Textfilen har skapats!".

## Fördjupning

När du skapar en textfil i Javascript är det viktigt att förstå att filen kommer att sparas där din kod och filen körs. Om du till exempel kör din kod från en mapp som heter "projekt", kommer textfilen att sparas i den mappen med namnet "mittTextdokument.txt".

Du kan också använda writeFile-funktionen för att ändra en befintlig textfil eller lägga till mer text i slutet av filen. Istället för att ange 'Det här är min första textfil', kan du skriva ett variabelnamn eller hämta text från en annan fil.

För att läsa innehållet i en textfil kan du använda readFile-funktionen. Här är ett exempel på hur du kan göra det:

```Javascript
// Läs innehållet i textfilen
fs.readFile('mittTextdokument.txt', 'utf8', function(err, data) {
  if (err) throw err;
  console.log(data);
});
```

Detta kommer att skriva ut innehållet i textfilen i ditt terminalfönster.

## Se också

För mer information om hur du kan använda textfiler i Javascript, kolla in dessa resurser:

- [En guide till att skriva och läsa textfiler i Node.js](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-node-js)
- [Dokumentation för fs-modulen i Node.js](https://nodejs.org/api/fs.html)