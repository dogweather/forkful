---
title:                "TypeScript: Sprawdzanie istnienia katalogu"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, na pewno wiesz, że czasem musisz sprawdzić, czy określony katalog istnieje. Jest to ważne, ponieważ pozwala uniknąć błędów w kodzie i zapewnia bezpieczeństwo działania aplikacji. W tym artykule dowiesz się, jak można sprawdzić istnienie katalogu w języku TypeScript.

## Jak to zrobić

Sprawdzenie istnienia katalogu w TypeScript jest bardzo proste. Wystarczy wykorzystać funkcję `existsSync` z modułu `fs`:

```TypeScript
import * as fs from 'fs';

if (fs.existsSync('ścieżka/do/katalogu')) {
    console.log('Katalog istnieje.');
} else {
    console.log('Katalog nie istnieje.');
}
```

Funkcja `existsSync` zwraca wartość logiczną `true` lub `false`, w zależności od tego, czy katalog istnieje czy nie. Jest to szybwy i bezpieczny sposób na sprawdzenie istnienia katalogu.

## Deep Dive

Jeśli chcesz się lepiej zaznajomić z tematem sprawdzania istnienia katalogów w TypeScript, warto zapoznać się z innymi funkcjami modułu `fs`. Na przykład, funkcja `statSync` zwraca informacje o pliku lub katalogu, w tym o jego istnieniu. Możesz również wykorzystać funkcję `readdirSync` do pobrania listy plików i katalogów w danym katalogu.

Dodatkowo, warto zwrócić uwagę na wyjątki, które mogą wystąpić podczas próby dostępu do katalogu. Zawsze należy odpowiednio obsłużyć błędy, aby uniknąć zatrzymywania działania aplikacji.

## Zobacz także

1. Dokumentacja modułu `fs`: https://nodejs.org/api/fs.html
2. Przykłady wykorzystania `existsSync`: https://www.geeksforgeeks.org/node-js-fs-existssync-method/#:~:text=The%20fs.,boolean%20value)%20to%20check%20existence.
3. Wykorzystanie funkcji `statSync`: https://www.w3schools.com/nodejs/nodejs_filesystem.asp