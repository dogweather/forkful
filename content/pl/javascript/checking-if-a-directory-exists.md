---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "Javascript: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy dany katalog istnieje, jest ważnym krokiem w procesie tworzenia aplikacji. Dzięki temu możemy mieć pewność, że nasz program będzie działał poprawnie i będzie mógł przechowywać i odczytywać potrzebne nam pliki.

## Jak to zrobić

Sprawdzenie istnienia katalogu w Javascript jest proste i wymaga użycia wbudowanej funkcji `fs.existsSync()`, która zwraca wartość boolean odpowiadającą na pytanie, czy dany katalog istnieje. Poniżej przedstawiam przykładowy kod oraz wynik działania funkcji:

```Javascript
const fs = require('fs');
// sprawdzenie istnienia katalogu "moj-katalog"
if (fs.existsSync('moj-katalog')) {
    console.log("Katalog istnieje!");
} else {
    console.log("Katalog nie istnieje.");
}
```

W przypadku, gdy katalog istnieje, wyświetli się komunikat "Katalog istnieje!". W przeciwnym razie, wyświetli się komunikat "Katalog nie istnieje.".

## Deep Dive

Podczas sprawdzania istnienia katalogu, należy pamiętać o kilku dodatkowych aspektach. Po pierwsze, funkcja `fs.existsSync()` sprawdza tylko istnienie katalogu w bieżącym katalogu lub w podanym ścieżce. Jeśli chcemy sprawdzić istnienie katalogu w innej lokalizacji, musimy podać pełną ścieżkę.

Kolejną rzeczą wartą uwagi jest zachowanie funkcji w przypadku, gdy podamy niepoprawną ścieżkę. W takim przypadku, zostanie wyrzucony błąd `Error`. Dlatego ważne jest, aby upewnić się, że podana ścieżka jest prawidłowa przed wywołaniem funkcji `fs.existsSync()`.

## Zobacz także

- Dokumentacja funkcji `fs.existsSync()`: https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_fs_existssync_path
- Przykładowy kod do sprawdzania istnienia katalogu: https://www.geeksforgeeks.org/node-js-fs-existssync-method/