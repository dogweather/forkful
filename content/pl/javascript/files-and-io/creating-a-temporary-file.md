---
date: 2024-01-20 17:40:53.662578-07:00
description: "How to: (Jak to zrobi\u0107?) U\u017Cyjemy modu\u0142u `fs` w Node.js\
  \ do pracy z plikami, a `os` do uzyskania \u015Bcie\u017Cki tymczasowego folderu."
lastmod: '2024-03-13T22:44:35.815649-06:00'
model: gpt-4-1106-preview
summary: "U\u017Cyjemy modu\u0142u `fs` w Node.js do pracy z plikami, a `os` do uzyskania\
  \ \u015Bcie\u017Cki tymczasowego folderu."
title: Tworzenie pliku tymczasowego
weight: 21
---

## How to: (Jak to zrobić?)
Użyjemy modułu `fs` w Node.js do pracy z plikami, a `os` do uzyskania ścieżki tymczasowego folderu.

```javascript
const fs = require('fs');
const os = require('os');
const path = require('path');

// Tworzenie unikalnej ścieżki do pliku tymczasowego
const tempDir = os.tmpdir();
const filePath = path.join(tempDir, `temp-file-${Date.now()}`);

// Zapis do pliku tymczasowego
fs.writeFile(filePath, 'Tutaj są moje dane tymczasowe!', (err) => {
  if (err) throw err;
  console.log(`Plik tymczasowy został zapisany: ${filePath}`);
  // Pamiętaj, by potem usunąć plik tymczasowy
  fs.unlink(filePath, (err) => {
    if (err) throw err;
    console.log(`Plik tymczasowy został usunięty: ${filePath}`);
  });
});
```

## Deep Dive (Wgłębmy się)
Pliki tymczasowe istnieją od lat. W Unix-like systemach zazwyczaj są przechowywane w `/tmp` i są automatycznie czyszczone przy resecie systemu. W Windows używa się `%TEMP%`. Node.js nie posiada wbudowanego mechanizmu do obsługi plików tymczasowych, więc tworzymy je ręcznie używając modułu `fs`.

Alternatywnie, można użyć modułu np. `tmp` z npm, który oferuje wyższy poziom abstrakcji:

```javascript
const tmp = require('tmp');

// Tworzenie pliku tymczasowego z automatycznym czyszczeniem
const tmpFile = tmp.fileSync({ keep: false, postfix: '.txt' });
console.log(`Plik tymczasowy: ${tmpFile.name}`);
// Zapisz, pracuj, zakończ - plik zniknie sam
```

Implementacja manualna, jak w pierwszym przykładzie, wymaga od programisty pamiętania o usunięciu pliku. Zaniedbanie tego może prowadzić do wycieku dyskowego.

## See Also (Zobacz również)
- Node.js `fs` moduł dokumentacji: https://nodejs.org/api/fs.html
- Moduł `tmp` dla szybkiego tworzenia plików tymczasowych: https://www.npmjs.com/package/tmp
- Dokumentacja Node.js `os` modułu: https://nodejs.org/api/os.html
- Jak Node.js obsługuje ścieżki: https://nodejs.org/api/path.html
