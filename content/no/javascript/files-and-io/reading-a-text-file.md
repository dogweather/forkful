---
title:                "Lese en tekstfil"
aliases: - /no/javascript/reading-a-text-file.md
date:                  2024-01-20T17:54:32.991132-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av tekstfil betyr å hente tekstdata fra en fil på disket. Programmerere gjør dette for å bearbeide data, konfigurere apper eller bare for å vise innholdet.

## Slik gjør du:
JavaScript kan lese tekstfiler med fil-APIen i nettlesere eller `fs`-modulen i Node.js. Her er eksempler på begge.

#### I nettleser:
```javascript
document.getElementById('fileInput').addEventListener('change', function(event) {
  let file = event.target.files[0];
  let reader = new FileReader();
  
  reader.onload = function(e) {
    console.log(e.target.result);
  };

  reader.readAsText(file);
});
```
HTML:
```html
<input type="file" id="fileInput">
```
Output:
```
Dette er innholdet i din tekstfil.
```

#### I Node.js:
```javascript
const fs = require('fs');

fs.readFile('eksempel.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```
Output:
```
Dette er innholdet i din tekstfil.
```

## Dypdykk
I gamle dager måtte man ofte bruke lavnivå språk for å lese filer. JavaScript, designet for nettlesere, hadde ingen innebygd måte å lese filer på disk før HTML5 og File API kom. I serverkontekst har Node.js sin `fs`-modul blitt standarden for filbehandling.

Det er alternativer til `fs`, som streams og promisified versjoner, som kan tilby bedre ytelse eller enklere syntaks for asynkrone operasjoner. Biblioteker som `fs-extra` eller `read-file-promise` blir også brukt.

Tekstfiler kan leses synkront, men det er lurt å bruke asynkron kode for ikke å blokkere event-løkken, spesielt når du arbeider med store filer eller i et I/O-intensivt miljø.

## Se Også
- MDN Web Docs for File API: [https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications)
- Node.js `fs` moduldokumentasjon: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- `fs-extra` npm-modul: [https://www.npmjs.com/package/fs-extra](https://www.npmjs.com/package/fs-extra)
