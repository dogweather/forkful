---
title:                "Tworzenie tymczasowego pliku"
html_title:           "Javascript: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tworzenie tymczasowych plików jest popularną praktyką wśród programistów, ponieważ pozwala na przechowywanie danych tymczasowych lub tworzenie plików konfiguracyjnych w trakcie wykonywania programu. Jest to szczególnie przydatne w przypadku, gdy dane nie są potrzebne po zakończeniu działania programu i nie chcemy utrudniać pracy naszemu systemowi poprzez pozostawienie niepotrzebnych plików.

## Jak to zrobić?

```Javascript
const fs = require('fs');
const path = require('path');

// Tworzenie pliku tymczasowego w folderze projektowym
const tempFile = path.join(__dirname, 'temp.txt');
// Zapis do pliku
fs.writeFileSync(tempFile, "To jest tymczasowy plik!");
// Odczyt z pliku
let tempData = fs.readFileSync(tempFile, 'utf-8');
console.log(tempData); //Output: To jest tymczasowy plik!

// Usuwanie pliku tymczasowego
fs.unlinkSync(tempFile);
```

## Głębsze zanurzenie

Tworzenie tymczasowych plików jest praktykowane od lat i jest wykorzystywane w różnych językach programowania. W JavaScript możemy użyć wbudowanej biblioteki fs do operacji na plikach, takich jak odczytywanie i zapisywanie danych. Jedną z alternatywnych metod jest wykorzystanie biblioteki os-tmpdir do generowania ścieżki do folderu tymczasowego.

## Zobacz również

- Dokumentacja Node.js dotycząca wbudowanej biblioteki fs: https://nodejs.org/api/fs.html
- Moduł npm os-tmpdir: https://www.npmjs.com/package/os-tmpdir