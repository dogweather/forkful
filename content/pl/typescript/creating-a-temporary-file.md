---
title:                "Tworzenie pliku tymczasowego"
html_title:           "TypeScript: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Tworzenie plików tymczasowych jest często niezbędne w procesie programowania. Są one przydatne w celu przechowywania tymczasowych danych lub wykonywania określonych operacji na plikach.

## Jak stworzyć plik tymczasowy w TypeScript

Tworzenie plików tymczasowych w TypeScript jest proste i wymaga użycia funkcji `fs.tmpfile()`. Poniżej znajduje się przykładowy kod, który tworzy plik tymczasowy i zapisuje do niego dane:

```TypeScript
import * as fs from 'fs';
const data = 'To jest przykładowe dane do zapisania w pliku tymczasowym';

// Tworzenie pliku tymczasowego
fs.tmpfile((err, tmpFile) => {
  if (err) throw err;

  // Zapisywanie danych do pliku
  fs.writeFile(tmpFile, data, (err) => {
    if (err) throw err;
    console.log('Plik tymczasowy został utworzony i zapisany.');
  });
});
```

W wyniku powyższego kodu, zostanie utworzony plik tymczasowy, a dane zostaną zapisane. Po zakończeniu kodu, plik tymczasowy zostanie automatycznie usunięty.

## Głębszy wgląd

Funkcja `fs.tmpfile()` jest dostępna w bibliotece Node.js i służy do tworzenia plików tymczasowych w systemie plików. Plik tymczasowy jest utworzony w katalogu tymczasowym systemu operacyjnego.

Istnieje również możliwość wykorzystania modułu `temp` z biblioteki `fs-extra`. Ten moduł oferuje większą kontrolę nad tworzeniem i usuwaniem plików tymczasowych.

## Zobacz też

- [Moduł fs w Node.js](https://nodejs.org/api/fs.html)
- [Moduł fs-extra w Node.js](https://github.com/jprichardson/node-fs-extra)