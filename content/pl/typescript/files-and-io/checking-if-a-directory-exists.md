---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:46.693454-07:00
description: "Jak to zrobi\u0107: TypeScript, uruchomiony w \u015Brodowisku Node.js,\
  \ umo\u017Cliwia sprawdzenie, czy katalog istnieje, za pomoc\u0105 modu\u0142u `fs`,\
  \ kt\xF3ry dostarcza funkcj\u0119\u2026"
lastmod: '2024-03-13T22:44:35.154058-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, uruchomiony w \u015Brodowisku Node.js, umo\u017Cliwia sprawdzenie,\
  \ czy katalog istnieje, za pomoc\u0105 modu\u0142u `fs`, kt\xF3ry dostarcza funkcj\u0119\
  \ `existsSync()` lub asynchroniczn\u0105 funkcj\u0119 `access()` po\u0142\u0105\
  czon\u0105 z `constants.F_OK`."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
TypeScript, uruchomiony w środowisku Node.js, umożliwia sprawdzenie, czy katalog istnieje, za pomocą modułu `fs`, który dostarcza funkcję `existsSync()` lub asynchroniczną funkcję `access()` połączoną z `constants.F_OK`.

### Używając `fs.existsSync()`:
```typescript
import { existsSync } from 'fs';

const directoryPath = './sciezka/do/katalogu';

if (existsSync(directoryPath)) {
  console.log('Katalog istnieje.');
} else {
  console.log('Katalog nie istnieje.');
}
```

### Używając `fs.access()` z `fs.constants.F_OK`:
```typescript
import { access, constants } from 'fs';

const directoryPath = './sciezka/do/katalogu';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Katalog nie istnieje.');
    return;
  }
  console.log('Katalog istnieje.');
});
```

**Przykładowy wynik** dla obu metod, zakładając, że katalog istnieje:
```
Katalog istnieje.
```

A jeśli nie istnieje:
```
Katalog nie istnieje.
```

### Używając biblioteki innej firmy - `fs-extra`:
`fs-extra` to popularna biblioteka innej firmy, która ulepsza wbudowany moduł `fs` i dostarcza bardziej wygodne funkcje.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './sciezka/do/katalogu';

pathExists(directoryPath).then(exists => {
  console.log(`Katalog istnieje: ${exists}`);
});
```

**Przykładowy wynik** gdy katalog istnieje:
```
Katalog istnieje: true
```

A jeśli nie:
```
Katalog istnieje: false
```
