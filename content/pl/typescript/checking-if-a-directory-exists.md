---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:59:01.054667-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
**Co i dlaczego?**

Sprawdzanie istnienia katalogu to zapytanie o to, czy folder naprawdę jest tam, gdzie go oczekujemy. Programiści robią to, by uniknąć błędów podczas pracy z plikami i katalogami, które mogłyby nie istnieć.

## How to:
**Jak to zrobić:**

```typescript
import * as fs from 'fs';
import * as path from 'path';

// Callback style
const directoryPath = path.join(__dirname, 'exampleDir');

fs.access(directoryPath, fs.constants.F_OK, (err) => {
  if (err) {
    console.error('Directory does not exist:', directoryPath);
  } else {
    console.log('Directory exists:', directoryPath);
  }
});

// Synchronous style
if (fs.existsSync(directoryPath)) {
  console.log('Directory exists:', directoryPath);
} else {
  console.error('Directory does not exist:', directoryPath);
}

// Async/Await with Promises
const checkDirectoryExists = async (dirPath: string): Promise<void> => {
  try {
    await fs.promises.access(dirPath, fs.constants.F_OK);
    console.log('Directory exists:', dirPath);
  } catch {
    console.error('Directory does not exist:', dirPath);
  }
};

// Usage
checkDirectoryExists(directoryPath);
```

Sample output when the directory does not exist:
```
Directory does not exist: /path/to/your/project/exampleDir
```

Sample output when the directory exists:
```
Directory exists: /path/to/your/project/exampleDir
```

## Deep Dive
**Wgłębienie się:**

W przeszłości sprawdzanie istnienia katalogu w Node.js odbywało się głównie za pomocą metod `fs.exists` lub `fs.existsSync`. Jednakże, `fs.exists` została uznana za przestarzałą (deprecated), ponieważ miała nieco mylące API.

Obecnie zaleca się korzystanie z `fs.access()` lub `fs.promises.access()` z odpowiednim flagą np. `fs.constants.F_OK`, aby sprawdzić dostępność pliku. Albo, jeśli potrzebujemy synchronicznej wersji, `fs.existsSync()` nadal jest dostępnym wyborem.

Alternatywami mogą być własne funkcje oparte na wyjątkach, które próbują coś zrobić z danym katalogiem i łapią błędy, jeśli katalog nie istnieje. Choć takie podejście może być w niektórych przypadkach bardziej "robuste", to jednak zwiększa złożoność kodu.

Dobrze zaprojektowana aplikacja sprawdza istnienie kluczowych katalogów na wczesnym etapie swojego uruchamiania, aby uniknąć niespodziewanych błędów w trakcie działania. 

## See Also
**Zobacz także:**

- Dokumentacja Node.js fs module: https://nodejs.org/api/fs.html
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
- Przewodnik po technikach asynchronicznych Node.js: https://nodejs.dev/learn/modern-asynchronous-javascript-with-async-and-await
