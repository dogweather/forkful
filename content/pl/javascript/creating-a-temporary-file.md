---
title:                "Javascript: Tworzenie pliku tymczasowego"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego tworzymy pliki tymczasowe?

Tworzenie tymczasowych plików jest wyjątkowo przydatne w programowaniu z wielu powodów. Może to być konieczne, jeśli chcemy zapisać pewne dane tymczasowo, ale nie trzeba trwale przechowywać ich w pliku. Innym powodem może być potrzeba pracy z danymi o różnych formatach, które muszą być tymczasowo przetworzone lub połączone w jednym miejscu. W tym artykule przedstawię Wam, jak stworzyć tymczasowy plik przy użyciu języka Javascript.

## Jak to zrobić?

Aby utworzyć tymczasowy plik w Javascript, musimy skorzystać z wbudowanej funkcji fs. Najpierw musimy zaimportować moduł "fs", a następnie użyć metody "writeFile" z tego modułu. Kod będzie wyglądał następująco:

```Javascript
const fs = require('fs');
fs.writeFile('temp.txt', 'To jest tymczasowy plik!', (err) => {
    if (err) throw err;
    console.log('Plik tymczasowy został utworzony!');
});
```

Po wykonaniu tego kodu, zostanie utworzony plik o nazwie "temp.txt" z zawartością "To jest tymczasowy plik!". Przykładowy output w konsoli wyglądałby tak:

```
Plik tymczasowy został utworzony!
```

Możemy również użyć metody "writeFileSync", która jest synchroniczna i w przypadku błędu wyrzuci wyjątek. Kod będzie wyglądał podobnie:

```Javascript
const fs = require('fs');
fs.writeFileSync('temp.txt', 'To jest tymczasowy plik!');
```

## Głębszy zanurzenie

Tworzenie tymczasowych plików jest kluczowym elementem w wielu projektach programistycznych. Dzięki takim plikom możemy tymczasowo przechowywać dane, przetwarzać je lub połączać, a następnie usunąć plik, gdy już nie jest nam potrzebny. Istnieją również inne metody w module "fs", które pozwalają na bardziej zaawansowane operacje na plikach. Możemy również używać biblioteki "tmp", która dostarcza wygodny interfejs do tworzenia i zarządzania tymczasowymi plikami.

## Zobacz również

- https://nodejs.org/api/fs.html
- https://www.npmjs.com/package/tmp
- https://www.w3schools.com/nodejs/nodejs_filesystem.asp