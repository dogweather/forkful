---
title:                "TypeScript: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś wyświetlić zawartość pliku tekstowego w swoim programie? Może chcesz przetworzyć dane zawarte w pliku lub po prostu je wyświetlić dla użytkownika. W tym artykule dowiesz się, jak odczytywać pliki tekstowe w języku TypeScript i jakie metody są dostępne do tego celu.

## Jak To Zrobić

W TypeScript możemy odczytać pliki tekstowe za pomocą wbudowanego modułu `fs`, który dostarcza metody do odczytu i zapisu plików. Najpierw musimy zaimportować ten moduł do naszego programu:

```TypeScript
import * as fs from 'fs';
```

Następnie możemy wywołać metodę `readFileSync`, która odczyta plik i zwróci jego zawartość jako ciąg znaków:

```TypeScript
const fileContents = fs.readFileSync('nazwa_pliku.txt', 'utf-8');
```

Teraz możemy wyświetlić zawartość pliku w konsoli lub przetwarzać ją dalej w naszym programie. W przypadku większych plików zaleca się korzystanie z metody `createReadStream`, która pozwala na odczyt pliku w postaci strumienia danych.

```TypeScript
const readStream = fs.createReadStream('nazwa_pliku.txt', 'utf-8');
readStream.on('data', (chunk) => {
  console.log(chunk);
});
```

## Głębszy Wgląd

Podczas odczytywania plików tekstowych w TypeScript możemy wykorzystać także wbudowany moduł `path`, który pozwala na operacje na ścieżkach plików. Możemy również skorzystać z wbudowanego modułu `util`, aby przetworzyć dane odczytane z pliku w postaci bufora do czytelniejszej formy.

## Zobacz też

- [Dokumentacja modułu fs](https://nodejs.org/api/fs.html)
- [Dokumentacja modułu path](https://nodejs.org/api/path.html)
- [Dokumentacja modułu util](https://nodejs.org/api/util.html)