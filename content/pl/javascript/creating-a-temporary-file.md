---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Javascript: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych może być niezbędne w niektórych sytuacjach podczas programowania w JavaScript. Pliki te są używane do przechowywania danych tymczasowych lub do przeprowadzenia testów w kodzie.

## Jak to zrobić

Aby utworzyć tymczasowy plik w JavaScript, możesz skorzystać z wbudowanej funkcji o nazwie `fs.mkdtemp()`. Przykładowy kod poniżej demonstruje użycie tej funkcji:
 
```Javascript
const fs = require('fs');

fs.mkdtemp('temp-', (err, folder) => {
  if (err) {
    throw err;
  }
  console.log(`Utworzono folder tymczasowy: ${folder}.`);
});
```

Po uruchomieniu powyższego kodu, powinieneś otrzymać wyjście w konsoli podobne do tego:

```
Utworzono folder tymczasowy: temp-467d4745c6f5f59a.
```

W powyższym przykładzie, nazwa pliku tymczasowego będzie zaczynać się od `temp-`, a następnie zostanie dodana unikalna kombinacja znaków wygenerowanych przez `fs.mkdtemp()`. Możesz również ustawić własny prefiks lub koniec nazwy pliku poprzez zmianę pierwszego argumentu funkcji `mkdtemp()`.

## Deep Dive

Funkcja `fs.mkdtemp()` pochodzi z wbudowanego modułu `fs` w Node.js, dzięki czemu możesz ją używać w dowolnym środowisku uruchomieniowym Node.js. Funkcja ta jest wykorzystywana do tworzenia folderów tymczasowych, a nie plików w ścisłym tego słowa znaczeniu. W przypadku potrzeby tworzenia plików tymczasowych, musisz wykorzystać inną funkcję o nazwie `fs.mkstemp()`, która również jest dostępna w module `fs`.

Należy pamiętać, że plik tymczasowy zostanie usunięty automatycznie po zakończeniu działania programu lub po wywołaniu `fs.rmdir()` lub `fs.unlink()`.

## Zobacz także

- [Oficjalna dokumentacja Node.js na temat tworzenia plików tymczasowych](https://nodejs.org/dist/latest/docs/api/fs.html#fs_fs_mkdtemp_prefix_options_callback)
- [Wprowadzenie do programowania w Node.js dla początkujących (po polsku)](https://codeburst.io/node-js-dla-pocz%C4%85tkuj%C4%85cych-wprowadzenie-650064d8b5c#.0s2cl4gmi)