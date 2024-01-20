---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:57:14.789912-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
"Co i dlaczego?"

Sprawdzanie, czy katalog istnieje, to proces weryfikacji ścieżki w systemie plików. Programiści robią to, aby uniknąć błędów podczas prób dostępu do nieistniejących katalogów czy zapisywania plików.

## How to:
"Jak to zrobić:"

Do sprawdzenia istnienia katalogu w Node.js używamy modułu `fs`. Przykłady:

Synchronicznie:

```javascript
const fs = require('fs');

const directoryPath = './path/to/directory';

if (fs.existsSync(directoryPath)) {
  console.log('Katalog istnieje!');
} else {
  console.log('Katalog nie istnieje!');
}
```

Asynchronicznie z `fs.promises`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Katalog istnieje!');
  } catch {
    console.log('Katalog nie istnieje!');
  }
}

checkDirectory('./path/to/directory');
```

## Deep Dive
"Dogłębna analiza"

W starszych wersjach Node.js, `fs.exists` był często używany do tego zadania, ale został wycofany z powodu wprowadzania w błąd. Asynchroniczne API, takie jak `fs.access` i `fs.stat`, są teraz zalecane. `fs.access` sprawdza uprawnienia, a `fs.stat` daje dodatkowe informacje o plikach/katalogach. Te funkcje są przydatne, ponieważ zapobiegają potencjalnym wyścigom, które mogą wystąpić, gdy stan systemu plików zmienia się między sprawdzeniami a operacjami.

## See Also
"Zobacz również"

- [Node.js File System Docs](https://nodejs.org/api/fs.html) - oficjalna dokumentacja modułu `fs`.
- [Working with file descriptors in Node.js](https://nodejs.org/dist/latest-v15.x/docs/api/fs.html#fs_working_with_file_descriptors) - jak pracować z deskryptorami plików.
- [Path Module in Node.js](https://nodejs.org/api/path.html) - moduł `path` do zarządzania ścieżkami plików.