---
title:                "Praca z plikami CSV"
aliases:
- /pl/javascript/working-with-csv/
date:                  2024-02-03T19:20:26.860407-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?
Praca z CSV (Comma-Separated Values, wartości oddzielone przecinkami) w JavaScript obejmuje parsowanie lub generowanie plików CSV w celu zaimportowania danych tabelarycznych z zewnętrznych źródeł lub eksportu danych do użycia w innych programach. Programiści robią to, ponieważ umożliwia to łatwą, lekką wymianę danych między aplikacjami, bazami danych i systemami, gdzie bardziej złożone formaty takie jak JSON mogą być nadmierną komplikacją.

## Jak to zrobić:
JavaScript nie ma wbudowanej funkcjonalności do parsowania lub stringifikacji CSV tak jak w przypadku JSON. Jednak można łatwo zarządzać danymi CSV, używając surowego JavaScriptu do prostszych zadań lub wykorzystując potężne biblioteki takie jak `PapaParse` do bardziej złożonych scenariuszy.

### Podstawowe parsowanie przy użyciu surowego JavaScriptu
Aby przeparsować prosty ciąg CSV na tablicę obiektów:

```javascript
const csv = `name,age,city
John,23,Nowy Jork
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
Wynik:

```
[
  { name: 'John', age: '23', city: 'Nowy Jork' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### Podstawowe tworzenie CSV przy użyciu surowego JavaScriptu
Aby przekształcić tablicę obiektów w ciąg CSV:

```javascript
const data = [
  { name: 'John', age: 23, city: 'Nowy Jork' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

Wynik:

```
John,23,Nowy Jork
Jane,28,Los Angeles
```

### Użycie PapaParse do złożonych zadań z CSV
Dla bardziej skomplikowanych scenariuszy `PapaParse` jest rozbudowaną biblioteką odpowiednią do parsowania i stringifikacji plików CSV z opcjami dla strumieni, workerów i obsługi dużych plików.

Parsowanie pliku CSV lub ciągu przy użyciu PapaParse:

```javascript
// Po dodaniu PapaParse do projektu
const Papa = require('papaparse');
const csv = `name,age,city
John,23,Nowy Jork
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Sparsowano:", results.data);
  }
});
```

Generuje:

```
Sparsowano: [
  ["name", "age", "city"],
  ["John", "23", "Nowy Jork"],
  ["Jane", "28", "Los Angeles"]
]
```

Stringifikacja tablicy do ciągu CSV przy użyciu PapaParse:

```javascript
const data = [
  { name: 'John', age: 23, city: 'Nowy Jork' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

Generuje:

```
name,age,city
John,23,Nowy Jork
Jane,28,Los Angeles
```

Te przykłady ilustrują podstawowe i zaawansowane obsługi plików CSV w JavaScript, umożliwiając łatwą wymianę danych w aplikacjach internetowych i nie tylko.
