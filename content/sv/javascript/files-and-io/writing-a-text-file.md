---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:15.708907-07:00
description: "Hur man g\xF6r: I en Node.js-milj\xF6 kan du anv\xE4nda den inbyggda\
  \ `fs` (File System)-modulen f\xF6r att skriva textfiler. Detta exempel demonstrerar\
  \ hur man\u2026"
lastmod: '2024-03-13T22:44:38.311671-06:00'
model: gpt-4-0125-preview
summary: "I en Node.js-milj\xF6 kan du anv\xE4nda den inbyggda `fs` (File System)-modulen\
  \ f\xF6r att skriva textfiler."
title: Att skriva en textfil
weight: 24
---

## Hur man gör:
I en Node.js-miljö kan du använda den inbyggda `fs` (File System)-modulen för att skriva textfiler. Detta exempel demonstrerar hur man skriver text till en fil asynkront:

```javascript
const fs = require('fs');

const data = 'Hej, världen! Det här är text som ska skrivas in i en fil.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('Filen har skrivits.');
});
```

Exempel på utdata:
```
Filen har skrivits.
```

För synkron filskrivning, använd `writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('Filen har skrivits.');
} catch (error) {
  console.error('Fel vid skrivning av fil:', error);
}
```

I moderna webbläsare introducerar File System Access API möjligheten att läsa och skriva filer. Dock är dess användning föremål för användarbehörigheter. Så här skapar och skriver du till en fil:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hej, världen! Det här är textfilsskrivning i webbläsare.');
  await writable.close();
}
```

För mer komplexa scenarion eller när du arbetar med stora filer, kanske du väljer att använda tredjepartsbibliotek som `FileSaver.js` för webbläsare:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hej, världen! Det här är text från FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

Kom ihåg, att skriva filer på klientens sida (i webbläsare) är begränsat på grund av säkerhetsproblem, och alla operationer som kräver att spara på användarens lokala disk kommer vanligtvis att kräva deras uttryckliga tillstånd.
