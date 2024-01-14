---
title:                "TypeScript: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w naszych programach musimy sprawdzić, czy dany katalog istnieje przed wykonaniem określonych operacji. Może to być przydatne, jeśli nasza aplikacja wymaga dostępu do plików lub innych zasobów przechowywanych w danym katalogu. W tym artykule dowiesz się, jak w prosty sposób sprawdzić istnienie katalogu w TypeScript i zapobiec błędom.

## Jak to zrobić 

Sprawdzenie istnienia katalogu w TypeScript jest bardzo proste. Wykorzystujemy do tego wbudowaną funkcję biblioteki `fs` o nazwie `existsSync()`. Przyjmujemy jako parametr ścieżkę do katalogu, który chcemy sprawdzić, a następnie w rezultacie otrzymujemy wartość boolean, która informuje nas, czy katalog istnieje czy nie. Poniżej przedstawione są dwa przykłady użycia tej funkcji: 

```TypeScript
import * as fs from "fs";

const directoryPath = "/sciezka/do/katalogu";

// Sprawdzenie istnienia katalogu 
// Spodziewamy się, że istnieje
console.log(fs.existsSync(directoryPath)); // Output: true

// Sprawdzenie istnienia katalogu 
// Spodziewamy się, że nie istnieje
console.log(fs.existsSync("/nie/istniejacy/katalog")); // Output: false
```

Możemy również wykorzystać tę funkcję w warunku `if`, aby wykonać odpowiednią akcję w zależności od istnienia katalogu:

```TypeScript
import * as fs from "fs";

const directoryPath = "/sciezka/do/katalogu";

if (fs.existsSync(directoryPath)) {
  console.log("Katalog istnieje!");
} else {
  console.log("Katalog nie istnieje!");
}
```

## Głębsza analiza

Funkcja `existsSync()` wykorzystuje podstawowe operacje systemowe, aby sprawdzić, czy katalog istnieje. Jeśli musimy dokonać bardziej zaawansowanych operacji na katalogu, może być konieczne wykorzystanie innych funkcji, takich jak `lstatSync()`, która zwraca informacje na temat katalogu, takie jak rozmiar, data utworzenia czy uprawnienia. Istnieje także wersja asynchroniczna tych funkcji, która przyjmuje trzeci argument w postaci funkcji zwrotnej, co pozwala na lepszą obsługę błędów.

## Zobacz również

- [Dokumentacja fs.existsSync() w Node.js](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Funkcje biblioteki fs w Node.js](https://nodejs.org/api/fs.html)

Dzięki temu prostemu sposobowi w TypeScript możemy łatwo sprawdzić, czy dany katalog istnieje, co może pomóc nam uniknąć błędów w naszych programach. Pamiętaj jednak, że w przypadku operacji na plikach i katalogach zawsze należy uważać i odpowiednio obsługiwać błędy.