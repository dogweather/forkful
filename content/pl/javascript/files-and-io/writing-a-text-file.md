---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:23.947725-07:00
description: "Jak to zrobi\u0107: W \u015Brodowisku Node.js mo\u017Cesz u\u017Cy\u0107\
  \ wbudowanego modu\u0142u `fs` (File System) do pisania plik\xF3w tekstowych. Ten\
  \ przyk\u0142ad demonstruje\u2026"
lastmod: '2024-03-13T22:44:35.814652-06:00'
model: gpt-4-0125-preview
summary: "W \u015Brodowisku Node.js mo\u017Cesz u\u017Cy\u0107 wbudowanego modu\u0142\
  u `fs` (File System) do pisania plik\xF3w tekstowych."
title: Pisanie pliku tekstowego
weight: 24
---

## Jak to zrobić:
W środowisku Node.js możesz użyć wbudowanego modułu `fs` (File System) do pisania plików tekstowych. Ten przykład demonstruje asynchroniczne pisanie tekstu do pliku:

```javascript
const fs = require('fs');

const data = 'Hello, World! To jest tekst, który zostanie zapisany w pliku.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('Plik został zapisany.');
});
```

Przykładowe wyjście:
```
Plik został zapisany.
```

Dla synchronicznego zapisu pliku użyj `writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('Plik został zapisany.');
} catch (error) {
  console.error('Błąd przy zapisie pliku:', error);
}
```

We współczesnych przeglądarkach internetowych interfejs API dostępu do systemu plików (File System Access API) wprowadza możliwość odczytywania i zapisywania plików. Jednak jego użycie jest uzależnione od pozwoleń użytkownika. Oto jak utworzyć i zapisać plik:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hello, World! To jest przykład zapisu pliku tekstowego w przeglądarce.');
  await writable.close();
}
```

Dla bardziej złożonych scenariuszy lub przy pracy z dużymi plikami, możesz zdecydować się na użycie bibliotek stron trzecich takich jak `FileSaver.js` dla przeglądarek:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hello, World! To jest tekst z FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

Pamiętaj, że zapisywanie plików po stronie klienta (w przeglądarkach) jest ograniczone ze względów bezpieczeństwa, i każda operacja wymagająca zapisu na lokalnym dysku użytkownika zazwyczaj wymaga jego wyraźnego pozwolenia.
