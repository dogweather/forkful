---
title:    "Javascript: Att skriva en textfil"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Varför
Att skriva en textfil kan vara ett användbart verktyg för programmerare när de behöver lagra textbaserad information på ett korrekt och strukturerat sätt. Det kan också användas för att skapa små databaser eller för att spara konfigurationsinställningar för en applikation.

# Så här gör du
För att skriva en textfil i Javascript, behöver vi först skapa ett nytt objekt av typen "File". Detta går att göra på flera sätt, men ett av de vanligaste är att använda "fs" (File System) modulen. Vi behöver först inkludera modulen genom att skriva `const fs = require('fs');` i början av vårt program.

Nästa steg är att öppna en fil genom att använda `fs.open()` funktionen. Vi anger sökvägen till filen som den första parametern och skriver sedan "w" som andra parameter för att ange att vi vill skriva data till filen. Sedan kan vi skriva vår text till filen med hjälp av `fs.write()` funktionen. Till exempel:

```Javascript
const fs = require('fs'); 

fs.open('textfil.txt', 'w', (err, file) => {
  if (err) throw err;
  fs.write(file, 'Det här är en textfil som skapats med hjälp av Javascript!', (err) => {
    if (err) throw err;
    console.log('Textfilen har skapats!');
  });
});
```

När detta är klart, behöver vi stänga vår fil genom att använda `fs.close()` funktionen. Det är också viktigt att hantera eventuella fel genom att använda `throw` eller `console.log` för att undvika problem med vår textfil.

# Deep Dive
Utöver att bara skriva en enkel textfil, finns det flera andra funktioner som kan användas för att manipulera textfiler i Javascript. Till exempel kan vi läsa en textfil genom att använda `fs.readFile()` funktionen och sedan skriva ut innehållet i konsolen.

```Javascript
const fs = require('fs');

fs.readFile('textfil.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Vi kan också få information om en fil, som till exempel dess storlek, genom att använda `fs.stat()` funktionen.

```Javascript
const fs = require('fs');

fs.stat('textfil.txt', (err, stats) => {
  if (err) throw err;
  console.log(`Storlek: ${stats.size} bytes`);
});
```

Det finns också andra funktioner som `fs.appendFile()` för att lägga till data till en befintlig fil, eller `fs.rename()` för att ändra namnet på en fil.

# Se också
- [Node.js API: File System](https://nodejs.org/api/fs.html)
- [W3Schools: Node.js File System Module](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Web Dev Simplified: How To Read And Write Text Files In Node.js](https://www.youtube.com/watch?v=5a0xGinNObo)