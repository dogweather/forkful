---
title:                "Javascript: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Dlaczego

Tworzenie tymczasowych plików jest nieodłącznym elementem programowania w języku Javascript. W wielu przypadkach, tworzenie tymczasowego pliku może być konieczne do poprawnego działania aplikacji. Jest to szczególnie przydatne przy pracy z plikami, bazami danych czy też podczas testowania aplikacji.

# Jak to zrobić

Aby utworzyć tymczasowy plik w języku Javascript, należy wykorzystać wbudowane funkcje z modułu `fs`. Poniżej przedstawiamy dwa przykłady, jeden dla systemów operacyjnych Windows, a drugi dla systemów Linux.

```Javascript
// Przykład dla systemu Windows

const fs = require('fs');
const path = require('path');

// Pobieramy ścieżkę do katalogu tymczasowego
const tempDir = os.tmpdir();

// Tworzymy nazwę tymczasowego pliku
const tempFileName = 'temp_file.txt';

// Tworzymy i zapisujemy plik w katalogu tymczasowym
fs.writeFile(path.join(tempDir, tempFileName), 'To jest treść tymczasowego pliku!', (err) => {
  if (err) throw err;
  console.log('Utworzono tymczasowy plik!');
});
```

```Javascript
// Przykład dla systemu Linux

const fs = require('fs');
const os = require('os');
const crypto = require('crypto');

// Pobieramy ścieżkę do katalogu tymczasowego
const tempDir = os.tmpdir();

// Tworzymy nazwę losowego tymczasowego pliku
const tempFileName = crypto.randomBytes(20).toString('hex') + '.txt';

// Tworzymy i zapisujemy plik w katalogu tymczasowym
fs.writeFile(path.join(tempDir, tempFileName), 'To jest treść tymczasowego pliku!', (err) => {
  if (err) throw err;
  console.log('Utworzono tymczasowy plik!');
});
```

Powyższe przykłady pokazują jak w prosty sposób można utworzyć oraz zapisać tymczasowy plik w języku Javascript. W obu przypadkach, po wykonaniu kodu, w katalogu tymczasowym pojawi się utworzony plik.

# Deep Dive

Tworzenie tymczasowych plików jest szczególnie przydatne podczas testowania aplikacji. Dzięki nim, można symulować działanie zapisu do plików, bez konieczności modyfikowania rzeczywistych plików. Ponadto, tymczasowe pliki pomagają w optymalizacji kodu, ponieważ ich tworzenie jest szybsze i mniej zasobożerne niż tworzenie i zapisywanie rzeczywistych plików na dysku.

W przypadku systemu operacyjnego Windows, do tworzenia tymczasowych plików można wykorzystać funkcję `mkstemp()` z modułu `fs`. Pozwala ona na utworzenie pliku oraz zwraca uchwyt do tego pliku, co pozwala na pracę z nim w dalszej części kodu.

Dla systemów Linux, można skorzystać z funkcji `randomBytes()` z modułu `crypto`. Generuje ona losowy ciąg znaków, który można wykorzystać jako nazwę tymczasowego pliku.

# Zobacz też

- Dokumentacja funkcji `fs` z oficjalnej strony Node.js: https://nodejs.org/api/fs.html
- Informacje o korzystaniu z katalogu tymczasowego w Node.js: https://nodejs.org/api/os.html#os_os_tmpdir