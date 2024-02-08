---
title:                "Pisanie pliku tekstowego"
date:                  2024-02-03T19:29:40.616971-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie pliku tekstowego"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie pliku tekstowego w TypeScript jest kluczową umiejętnością dla trwałości danych, konfiguracji lub generowania logów. Programiści często wykonują to zadanie, aby przechowywać i manipulować danymi poza pamięcią aplikacji z powodów takich jak analiza danych, raportowanie lub po prostu zapisywanie ustawień użytkownika między sesjami.

## Jak to zrobić:
Sam TypeScript nie obsługuje bezpośrednio operacji na plikach, ponieważ kompiluje się do JavaScript, który tradycyjnie jest uruchamiany w przeglądarce z ograniczonym dostępem do systemu plików. Jednakże, gdy używany jest w środowisku Node.js, moduł `fs` (File System) zapewnia funkcjonalność zapisywania plików.

### Korzystanie z modułu fs Node.js
Najpierw upewnij się, że pracujesz w środowisku Node.js. Następnie użyj modułu `fs` do zapisywania plików tekstowych. Oto podstawowy przykład:

```typescript
import * as fs from 'fs';

const data = 'Hello, world!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('Plik został zapisany!');
});
```

To asynchronicznie zapisze "Hello, world!" do `message.txt`. Jeśli plik nie istnieje, Node.js go tworzy; jeśli istnieje, Node.js go nadpisuje.

Do synchronicznego zapisywania plików użyj `writeFileSync`:

```typescript
import * as fs from 'fs';

const data = 'Hello again, world!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('Plik został zapisany!');
} catch (err) {
    console.error(err);
}
```

### Korzystanie z popularnych bibliotek firm trzecich
Chociaż natywny moduł `fs` jest potężny, niektórzy programiści wolą korzystać z bibliotek firm trzecich dla dodatkowej wygody i funkcjonalności. `fs-extra` jest popularnym wyborem, który rozszerza `fs` i upraszcza operacje na plikach.

Najpierw musisz zainstalować `fs-extra`:

```
npm install fs-extra
```

Następnie możesz go użyć w swoim pliku TypeScript do zapisywania treści tekstowej:

```typescript
import * as fs from 'fs-extra';

const data = 'To jest fs-extra!';
const filePath = './extraMessage.txt';

// Korzystając z async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('Plik został zapisany za pomocą fs-extra!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

Ten fragment kodu robi to samo co wcześniejsze przykłady z `fs`, ale wykorzystuje bibliotekę `fs-extra`, oferując czystrzą składnię do obsługi obietnic.
