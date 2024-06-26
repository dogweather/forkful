---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:48.880318-07:00
description: "Jak to zrobi\u0107: W Node.js, poniewa\u017C JavaScript sam w sobie\
  \ nie ma bezpo\u015Bredniego dost\u0119pu do systemu plik\xF3w, zazwyczaj u\u017C\
  ywany jest modu\u0142 `fs` do tego typu\u2026"
lastmod: '2024-03-13T22:44:35.810792-06:00'
model: gpt-4-0125-preview
summary: "W Node.js, poniewa\u017C JavaScript sam w sobie nie ma bezpo\u015Bredniego\
  \ dost\u0119pu do systemu plik\xF3w, zazwyczaj u\u017Cywany jest modu\u0142 `fs`\
  \ do tego typu operacji."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
W Node.js, ponieważ JavaScript sam w sobie nie ma bezpośredniego dostępu do systemu plików, zazwyczaj używany jest moduł `fs` do tego typu operacji. Oto prosty sposób, aby sprawdzić, czy katalog istnieje, używając `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Sprawdź, czy katalog istnieje
if (fs.existsSync(directoryPath)) {
  console.log('Katalog istnieje.');
} else {
  console.log('Katalog nie istnieje.');
}

```
**Przykładowy wynik:**
```
Katalog istnieje.
```
Lub, dla podejścia nieblokującego i asynchronicznego, użyj `fs.promises` z `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Katalog istnieje.');
  } catch (error) {
    console.log('Katalog nie istnieje.');
  }
}

checkDirectory('./sample-directory');
```
**Przykładowy wynik:**
```
Katalog istnieje.
```

Dla projektów, które intensywnie korzystają z operacji na plikach i katalogach, pakiet `fs-extra`, rozszerzenie natywnego modułu `fs`, oferuje wygodne dodatkowe metody. Oto jak możesz osiągnąć to samo z `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Sprawdź, czy katalog istnieje
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Katalog istnieje.' : 'Katalog nie istnieje.'))
  .catch(err => console.error(err));
```
**Przykładowy wynik:**
```
Katalog istnieje.
```

To podejście umożliwia czysty i czytelny kod, który bezproblemowo integruje się z nowoczesnymi praktykami JavaScript.
