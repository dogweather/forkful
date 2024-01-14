---
title:    "Javascript: Tworzenie pliku tymczasowego"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego tworzyć plik tymczasowy w Javascript?

Tworzenie plików tymczasowych jest ważnym aspektem programowania w Javascripcie. Są one używane do przechowywania danych tymczasowych, które są potrzebne w trakcie działania programu. Są one również wykorzystywane do przechowywania danych, które nie są potrzebne na stałe lub potrzebne są tylko na chwilę.

## Jak to zrobić?

Tworzenie plików tymczasowych w Javascripcie jest stosunkowo proste. Można tego dokonać przy użyciu wbudowanych modułów takich jak "fs" lub "tmp". Oto przykładowy kod wraz z wyjaśnieniem:

```Javascript
// Importowanie modułu "fs", który pozwala na operacje na plikach
const fs = require('fs');

// Importowanie modułu "tmp", który umożliwia tworzenie plików tymczasowych
const tmp = require('tmp');

// Ustawienie nazwy i rozszerzenia pliku tymczasowego
const tempFile = tmp.fileSync({ prefix: 'temp', postfix: '.txt' });

// Zapisanie danych do pliku tymczasowego przy użyciu metody "writeFileSync" z modułu "fs"
fs.writeFileSync(tempFile.name, 'To jest przykładowy tekst');

// Odczytanie danych z pliku tymczasowego przy użyciu metody "readFileSync" z modułu "fs"
const data = fs.readFileSync(tempFile.name);

// Wyświetlenie danych w konsoli
console.log(data) // Output: Buffer[To jest przykładowy tekst]
```

## Pogłębione wskazówki

Można również ustawić inne opcje podczas tworzenia pliku tymczasowego, na przykład ustawienie miejsca zapisu, rozmiaru lub prawa dostępu. Więcej informacji na ten temat można znaleźć w dokumentacji modułu "tmp". Ważnym aspektem jest również usunięcie pliku tymczasowego po zakończeniu jego użytkowania. W tym celu można użyć metody "removeCallback" z modułu "tmp".

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o operacjach na plikach w Javascripcie, możesz zapoznać się z poniższymi linkami:

- [Dokumentacja modułu "fs"](https://nodejs.org/api/fs.html)
- [Dokumentacja modułu "tmp"](https://github.com/tmpfs/tmp)