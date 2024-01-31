---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapisywanie pliku tekstowego to proces tworzenia lub modyfikowania danych w formacie czytelnym dla człowieka. Programiści robią to, aby zapisywać konfiguracje, logi lub wymieniać dane między systemami.

## Jak to zrobić:
Node.js umożliwia pracę z plikami. Użyj `fs` do zapisu:

```Javascript
const fs = require('fs');

let data = "Cześć! To przykładowy tekst.";

// Zapisz do nowego pliku
fs.writeFile('przykladowy.txt', data, (err) => {
  if (err) throw err;
  console.log('Plik został zapisany!');
});

// Zapisz do istniejącego pliku
fs.appendFile('przykladowy.txt', '\nDo widzenia!', (err) => {
  if (err) throw err;
  console.log('Tekst został dopisany!');
});
```

W przeglądarkach użyj `Blob` i `URL.createObjectURL()`:

```Javascript
let data = new Blob(["Cześć! To tekst w pliku."], { type: 'text/plain' });
let textFile = window.URL.createObjectURL(data);

// Pobierz URL do świeżo utworzonego pliku
console.log('URL pliku:', textFile);
```

## Deep Dive
W Node.js, `fs` jest od lat, ale można użyć też `fs/promises` dla async/await. Przeglądarki wcześniej polegały na `FileReader` i `document.execCommand()`, ale Blob i `createObjectURL()` są nowocześniejsze. Warto też znać `fs.readFileSync()` i `fs.writeFileSync()` dla synchronicznej pracy z plikami.

## Zobacz również
- [Node.js `fs` documentation](https://nodejs.org/api/fs.html)
- [MDN Web Docs on Blobs](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
- [HTML Living Standard for `createObjectURL`](https://html.spec.whatwg.org/multipage/browsers.html#dom-url-createobjecturl)
