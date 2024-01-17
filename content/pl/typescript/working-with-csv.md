---
title:                "Praca z plikami csv"
html_title:           "TypeScript: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego warto?

Praca z plikami CSV jest nieodłączną częścią życia dewelopera. CSV (z ang. Comma-Separated Values) to format pliku, który służy do przechowywania danych w sposób oddzielony przecinkami. Jest to powszechnie używany format do przechowywania i przetwarzania danych, dlatego programiści często muszą umieć pracować z plikami CSV.

## Jak to zrobić?

Poniżej przedstawiamy kilka przykładów kodu w języku TypeScript oraz odpowiadające im wyniki.

```TypeScript
// Tworzenie obiektu CSV z tablicy danych
const csv = require('csvtojson');
const data = [['John', 25, 'Male'], ['Jane', 30, 'Female']];
const csvString = csv.fromArray(data).then(str => console.log(str));

// Wynik: "John,25,Male\nJane,30,Female"

// Parsowanie pliku CSV na tablicę obiektów
const csv = require('csvtojson');
const fs = require('fs');
const csvFilePath = 'dane.csv';
csv().fromFile(csvFilePath)
    .then((jsonObj) => {
        console.log(jsonObj);
    });

// W pliku CSV: "Imię,Wiek,Płeć\nJohn,25,Male\nJane,30,Female"
// Wynik: [{ 'Imię': 'John', Wiek: 25, Płeć: 'Male' },{ 'Imię': 'Jane', Wiek: 30, Płeć: 'Female' }]
```

## Głębsze zagadnienia

Pliki CSV mają swoje korzenie w archaicznym formacie Standard Generalized Markup Language (SGML) z lat 60., jednak w 70. stały się bardziej popularne dzięki programowi Microsoft Excel. Alternatywą dla plików CSV jest format pliku Excel (.xls lub .xlsx), jednak pliki CSV są uniwersalniejsze, ponieważ są wspierane przez większość programów. Przy pracy z plikami CSV należy pamiętać o encodingu, ponieważ pliki w różnych językach mogą mieć różne encodowanie.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pracy z plikami CSV w TypeScript, polecamy dokumentację [csvtojson library](https://www.npmjs.com/package/csvtojson) oraz [oficjalny tutorial](https://codeforgeek.com/read-write-csv-file-node-js/). Alternatywą dla biblioteki csvtojson jest [fast-csv](https://github.com/C2FO/fast-csv), która również jest łatwa w użyciu i ma dużą wydajność.