---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Javascript: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Sprawdzanie czy katalog istnieje w JavaScript

## Co i dlaczego?

Sprawdzanie, czy katalog istnieje, polega na potwierdzeniu, że na dysku twardym istnieje określony folder. Programiści robią to, aby uniknąć błędów, kiedy pliki są zapisywane lub odczytywane z miejsc, które faktycznie nie istnieją.

## Jak to zrobić?

Oto przykładowy kod, jak to zrobić w Node.js:

```Javascript
const fs = require('fs');
const folderPath = './path/to/your/directory';

if (fs.existsSync(folderPath)) {
    console.log('Katalog istnieje!');
} else {
    console.log('Katalog nie istnieje!');
}
```

Po uruchomieniu tego kodu, otrzymamy wiadomość mówiącą nam, czy katalog istnieje, czy nie.

## Więcej szczegółów

W kontekście historycznym, sprawdzanie czy katalog istnieje w JavaScript nie zawsze było możliwe. Dopiero od wprowadzenia Node.js i modułu `fs`, zdolność do interakcji z systemem plików stała się możliwa.

Co do implementacji, metoda `fs.existsSync()`  wymaga jednego argumentu: ścieżki do katalogu, który chcesz sprawdzić.

Jest też alternatywa do `fs.existsSync()`, to `fs.stat()` lub `fs.access()`. Ale `fs.existsSync()` jest bardziej bezpośredni, bo zwraca tylko `true` albo `false`.

## Zobacz też

1. [Dokumentacja Node.js fs](https://nodejs.org/api/fs.html)
2. [Porównanie fs.existsSync() z fs.access()](https://nodejs.dev/learn/check-if-a-file-exists-in-nodejs)

W pracy z katalogami takimi jak te, pamiętaj, aby zawsze być precyzyjnym w ścieżkach, którymi się poruszasz. Kod, który działa poprawnie na jednym komputerze, może zwrócić błędy na innym, jeżeli katalogi są inaczej strukturyzowane.