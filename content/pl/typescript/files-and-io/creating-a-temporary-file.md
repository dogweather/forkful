---
date: 2024-01-20 17:41:39.967018-07:00
description: "How to: W TypeScript korzystamy z paczki `fs` z Node.js, \u017Ceby \u0142\
  atwo zarz\u0105dza\u0107 plikami \u2014 tworzy\u0107, odczytywa\u0107, pisa\u0107\
  \ i usuwa\u0107, tak\u017Ce te tymczasowe."
lastmod: '2024-03-13T22:44:35.159408-06:00'
model: gpt-4-1106-preview
summary: "W TypeScript korzystamy z paczki `fs` z Node.js, \u017Ceby \u0142atwo zarz\u0105\
  dza\u0107 plikami \u2014 tworzy\u0107, odczytywa\u0107, pisa\u0107 i usuwa\u0107\
  , tak\u017Ce te tymczasowe."
title: Tworzenie pliku tymczasowego
weight: 21
---

## How to:
W TypeScript korzystamy z paczki `fs` z Node.js, żeby łatwo zarządzać plikami — tworzyć, odczytywać, pisać i usuwać, także te tymczasowe.

```typescript
import fs from 'fs';
import os from 'os';
import path from 'path';

function createTempFile(prefix: string): string {
  // Stwórz unikalną ścieżkę bazując na czasie i prefixie
  const tempPath = path.join(os.tmpdir(), `${prefix}${Date.now()}`);
  
  // Stwórz plik i zwróć ścieżkę
  fs.writeFileSync(tempPath, 'Moje tymczasowe dane');
  return tempPath;
}

// Użycie funkcji:
const tempFilePath = createTempFile('myAppTemp');
console.log(`Plik tymczasowy został utworzony w: ${tempFilePath}`);

// Sprzątanie po skończonej pracy
fs.unlinkSync(tempFilePath);
console.log(`Plik tymczasowy został usunięty.`);
```

Output:

```
Plik tymczasowy został utworzony w: /tmp/myAppTemp1612331122337
Plik tymczasowy został usunięty.
```

## Deep Dive
Pliki tymczasowe nie są nowością; istnieją od dawna, ułatwiając programowanie aplikacji wymagających tymczasowej pamięci pomocniczej. Alternatywy dla plików tymczasowych obejmują używanie baz danych w pamięci lub specjalnych systemów plików w pamięci RAM, jak tmpfs w systemach typu Unix.

Przy tworzeniu pliku tymczasowego ważne jest, by zapewnić jego unikatowość, najlepiej przez dodanie znacznika czasowego lub używając funkcji generujących unikalne identyfikatory. Zwróć uwagę na odpowiednie zarządzanie życiem pliku tymczasowego — nie chcesz, by śmieci zalegały twoim systemie.

## See Also
1. Node.js File System API: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
2. `os.tmpdir()` dokumentacja: [https://nodejs.org/api/os.html#ostmpdir](https://nodejs.org/api/os.html#ostmpdir)
3. `path` moduł dokumentacja: [https://nodejs.org/api/path.html](https://nodejs.org/api/path.html)
