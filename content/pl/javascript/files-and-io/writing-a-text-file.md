---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:23.947725-07:00
description: "Pisanie pliku tekstowego w JavaScript cz\u0119sto odnosi si\u0119 do\
  \ tworzenia i zapisywania danych w prostym, czytelnym formacie dla cel\xF3w logowania,\
  \ eksportowania\u2026"
lastmod: '2024-03-11T00:14:09.025804-06:00'
model: gpt-4-0125-preview
summary: "Pisanie pliku tekstowego w JavaScript cz\u0119sto odnosi si\u0119 do tworzenia\
  \ i zapisywania danych w prostym, czytelnym formacie dla cel\xF3w logowania, eksportowania\u2026"
title: Pisanie pliku tekstowego
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie pliku tekstowego w JavaScript często odnosi się do tworzenia i zapisywania danych w prostym, czytelnym formacie dla celów logowania, eksportowania danych użytkownika lub konfiguracji. Ta funkcjonalność jest kluczowa dla aplikacji, które muszą zachować dane poza czasem życia procesu aplikacji, dostarczając sposób na przechowywanie i późniejsze pobieranie lub udostępnianie informacji.

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
