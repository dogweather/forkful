---
title:                "Praca z plikami csv"
html_title:           "Javascript: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Co to jest CSV & Dlaczego?

Praca z CSV to jedna z częstych zadań programistycznych. CSV to format pliku przechowującego dane w postaci tabelarycznej. Jest to popularny sposób przechowywania i udostępniania danych w wielu dziedzinach, takich jak finanse, nauka, biznes, itp. Programiści często pracują z plikami CSV, aby przetwarzać i manipulować danymi w swoich projektach.

## Jak to zrobić:

Aby pracować z plikami CSV w Javascript, potrzebujesz odpowiednich narzędzi oraz znajomości niektórych funkcji języka. Możesz użyć wbudowanej funkcji ```require()``` do wczytania biblioteki ```csv-parser```. Następnie użyj funkcji ```createReadStream()``` aby utworzyć strumień z pliku CSV, a następnie przekazuj dane do funkcji ```csv-parser()```, która przetwarza plik i zwraca dane w postaci tablicy obiektów. Poniżej znajduje się przykładowy kod i jego wynik:

```Javascript
const csv = require('csv-parser');
const fs = require('fs'); // wbudowana biblioteka w Node.js do pracy z plikami
const results = [];

fs.createReadStream('plik.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results); // [ { kolumna1: 'wartość1', kolumna2: 'wartość2' }, ... ]
  });
```

## Deep Dive:

Format CSV został opracowany w 1972 roku przez Petera Naur-a, aby ułatwić przechowywanie i przetwarzanie danych w systemach telekomunikacyjnych. Obecnie jest szeroko stosowany dla przechowywania danych tabelarycznych w elektronicznej postaci. Istnieją również inne sposoby przechowywania danych tabelarycznych, takie jak XML, JSON czy Excel, jednak CSV jest wygodnym formatem dla prostych i dużych zbiorów danych.

Pracując z CSV, należy zwrócić uwagę na ustawienie odpowiednich parametrów, takich jak separator, nagłówki kolumn czy kodowanie pliku. W celu bardziej zaawansowanego przetwarzania danych można również używać bibliotek, takich jak ```csv-writer``` lub ```csvtojson```.

## Zobacz też:

- [Przykład użycia biblioteki ```csv-parser```](https://www.npmjs.com/package/csv-parser)
- [Inne popularne biblioteki do pracy z CSV](https://www.npmjs.com/search?q=CSV)
- [Oficjalna dokumentacja formatu CSV](https://tools.ietf.org/html/rfc4180)