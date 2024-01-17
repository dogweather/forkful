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

## Czym jest tworzenie tymczasowego pliku i dlaczego programiści to robią?

Tworzenie tymczasowego pliku jest procesem tworzenia pliku, który jest używany przez program tylko tymczasowo, a następnie jest usuwany. Programiści często tworzą tymczasowe pliki, gdy potrzebują przechowywać tymczasowe dane lub gdy wymagana jest tymczasowa lokalizacja dla pliku.

## Jak to zrobić?

```typescript
// Importuje moduł fs z biblioteki standardowej
import * as fs from 'fs';

// Utwórz tymczasowy plik z użyciem metody mkstemp
fs.mkstemp('temporary-file-', (err, tempFile) => {
  if (err) throw err;
  // Początkowy kawałek kodu, który używa tymczasowego pliku
  console.log(`Utworzono tymczasowy plik: ${tempFile}`);
  // Usuwa tymczasowy plik
  fs.unlink(tempFile, (err) => {
    if (err) throw err;
    console.log(`Usunięto tymczasowy plik: ${tempFile}`);
  });
})
```

W powyższym przykładzie wykorzystujemy moduł "fs" z biblioteki standardowej TypeScript, aby utworzyć tymczasowy plik o nazwie "temporary-file-XXXXXX" i wyświetlić jego nazwę po utworzeniu. Następnie, w kolejnym kroku, usuwamy tymczasowy plik z użyciem metody "unlink". 

## Głębszy wgląd

### Kontekst historyczny

Tworzenie tymczasowych plików jest powszechnie stosowane w systemach operacyjnych od lat 60. Początkowo używano ich jako jednego z mechanizmów do zarządzania pamięcią w systemie. Obecnie, programiści używają tymczasowych plików do wielu różnych celów, takich jak przechowywanie danych, synchronizacja procesów czy wykonywanie operacji na plikach w bezpieczny sposób.

### Alternatywy

Alternatywą dla tworzenia tymczasowych plików może być użycie zmiennych (np. zmiennych środowiskowych) lub wykorzystanie specjalnych rozwiązań dostarczanych przez frameworki lub biblioteki. Jednak tworzenie tymczasowych plików jest prostsze i pozostaje powszechnie stosowane w programowaniu.

### Szczegóły implementacyjne

Podczas tworzenia tymczasowego pliku, system operacyjny generuje unikalną nazwę dla pliku, zwykle w formacie "nazwa_tymczasowa_XXXXXX". Już po utworzeniu tymczasowego pliku, innym procesom nie jest udzielony dostęp do tego pliku, co zapewnia bezpieczne wykorzystanie go przez program.

## Zobacz także

- [Dokumentacja Node.js - moduł fs](https://nodejs.org/api/fs.html#fs_fs_mkstemp_prefix_options_callback)
- [Tworzenie tymczasowych plików w Pythonie](https://blog.dbrgn.ch/2013/3/26/temporary-files-in-python/)