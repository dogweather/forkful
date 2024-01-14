---
title:                "Javascript: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Najczęstszym zadaniem programisty jest manipulowanie plikami i folderami. W niektórych przypadkach możemy konieczne jest sprawdzenie, czy dany folder istnieje w systemie plików. W tym artykule dowiesz się, jak w prosty sposób możesz to zrobić w języku Javascript.

## Jak to zrobić

Sprawdzenie istnienia folderu w systemie plików w języku Javascript jest bardzo prostym zadaniem. Wystarczy skorzystać z metody `existsSync()` z modułu `fs`. Poniżej przedstawiamy przykładowy kod oraz jego wyjście:

```Javascript
const fs = require('fs');

const directoryPath = './test';

if (fs.existsSync(directoryPath)) {
  console.log('Folder istnieje');
} else {
  console.log('Folder nie istnieje');
}
```

W powyższym przykładzie korzystamy z metod `require()` do zaimportowania modułu `fs`. Następnie, w zmiennej `directoryPath` przechowujemy ścieżkę do folderu, którego istnienie chcemy sprawdzić. W instrukcji warunkowej wykorzystujemy metodę `existsSync()`, która zwróci wartość `true`, jeśli folder istnieje, lub `false`, jeśli nie istnieje. W zależności od tego wyświetlamy odpowiedni komunikat.

Przykładowe wyjście w przypadku, gdy folder istnieje:

```
Folder istnieje
```

A wyjście, gdy folder nie istnieje:

```
Folder nie istnieje
```

## Deep Dive

W bardziej zaawansowanej wersji możemy skorzystać z metody `access()` z modułu `fs`, która umożliwia nie tylko sprawdzenie, czy dany folder istnieje, ale również czy mamy do niego dostęp. Metoda ta przyjmuje dwa argumenty: ścieżkę do folderu oraz flagę, która określa rodzaj dostępu, jaki chcemy sprawdzić. Dostępne flagi to `fs.constants.F_OK` (sprawdza, czy plik istnieje), `fs.constants.R_OK` (sprawdza, czy plik jest dostępny do odczytu), `fs.constants.W_OK` (sprawdza, czy plik jest dostępny do zapisu) oraz `fs.constants.X_OK` (sprawdza, czy plik jest wykonywalny). Poniżej przedstawiamy przykładowy kod oraz jego wyjście:

```Javascript
const fs = require('fs');

const directoryPath = './test';

fs.access(directoryPath, fs.constants.R_OK, (err) => {
  if (err) {
    console.log('Folder nie istnieje lub nie ma dostępu do odczytu');
  } else {
    console.log('Folder istnieje i jest dostępny do odczytu');
  }
});
```

W tym przykładzie korzystamy z funkcji zwrotnej przekazanej jako trzeci argument do metody `access()`. Jeśli funkcja ta zostanie wywołana z błędem, oznacza to, że folder nie istnieje lub nie mamy do niego dostępu. W przeciwnym razie, jeśli nie ma błędu, oznacza to, że folder istnieje i jest dostępny do odczytu.

Przykładowe wyjście w przypadku, gdy folder istnieje i jest dostępny do odczytu:

```
Folder istnieje i jest dostępny do odczytu
```

A wyjście, gdy folder nie istnieje lub nie ma dostępu do odczytu:

```
Folder nie istnieje lub nie ma dostępu do odczytu
```

## Zobacz również

- Dokumentacja modułu `fs` w języku Javascript: https://nodejs.org/api/fs.html
- Przykładowy kod sprawdzający istnienie folderu na platformie CodeSandbox: https://codesandbox.io/s/check-directory-existence-muhdt