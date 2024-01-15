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

## Dlaczego
Możliwe powody do wykorzystania plików CSV w codziennej pracy są niezliczone. Czy to aby prosto przechowywać i przesyłać dane, czy też szybko przetwarzać je w programie - format CSV jest niezwykle wszechstronny i przydatny w różnych dziedzinach pracy.

## Jak To Zrobić
Nie ma nic prostszego niż praca z plikami CSV w języku Javascript. Wystarczy użyć odpowiedniej biblioteki lub modułu, a cały proces będzie szybki i łatwy. Poniżej przedstawiamy przykłady kodów i wyjść dla różnych operacji na plikach CSV.

Tworzenie obiektu CSV z danymi:
```Javascript
const csv = require('csvtojson');

const csvData = [
    { name: 'John', age: 25, profession: 'Programmer' },
    { name: 'Anna', age: 30, profession: 'Designer' },
    { name: 'Mark', age: 35, profession: 'Manager' }
]

const csvString = csvData.map(data => Object.values(data).join(',')).join('\n');
console.log(csvString);
// Output: "John,25,Programmer
// Anna,30,Designer
// Mark,35,Manager"
```

Przetwarzanie istniejącego pliku CSV i zapisywanie nowych danych:
```Javascript
const fs = require('fs');
const csv = require('csvtojson');

// Read CSV file
fs.readFile('data.csv', 'utf8', (err, data) => {
    if (err) throw err;

    // Convert CSV to JSON
    csv().fromString(data).then((json) => {
        // Manipulate JSON data
        json.push({ name: 'Sarah', age: 27, profession: 'Writer' });

        // Convert JSON to CSV
        const csvString = json.map(data => Object.values(data).join(',')).join('\n');

        // Write new CSV file
        fs.writeFile('new_data.csv', csvString, 'utf8', (err) => {
            if (err) throw err;
            console.log('New CSV file created!');
        });
    });
});
```

## Głębsza Wiedza
Pliki CSV są bardzo popularne w kontekście przetwarzania danych numerycznych i tekstowych, ale mogą również zawierać dane w formacie JSON. W przypadku gdy CSV ma nagłówki kolumn, można użyć opcji `noheader` podczas przetwarzania do JSON, aby uzyskać dane w postaci obiektów z kluczami odpowiadającymi nazwom kolumn.

Do zaawansowanej manipulacji danymi w plikach CSV można wykorzystać również biblioteki takie jak `csv-parser` czy `fast-csv`, które oferują dodatkowe funkcje takie jak filtrowanie lub grupowanie danych.

## Zobacz Również
- [Dokumentacja biblioteki CSV do konwersji danych](https://csv.js.org/)
- [Poradnik z przykładami wykorzystania CSV w języku Javascript](https://www.pluralsight.com/guides/import-export-csv-files-javascript)
- [Przewodnik po manipulacji danymi w plikach CSV w języku Javascript](https://stephanwagner.me/only-three-steps-to-read-and-write-csv-files-in-javascript)