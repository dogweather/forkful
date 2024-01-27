---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
CSV, czyli wartości oddzielone przecinkami, to prosty format wymiany danych. Programiści używają go ze względu na prostotę i uniwersalność – praktycznie każdy język programowania i baza danych rozumie CSV.

## Jak to zrobić?
```Javascript
// Parsowanie CSV:
const parseCSV = (csvString) => {
  const lines = csvString.split("\n");
  return lines.map(line => line.split(","));
};

const csvData = "name,age\nJohn,30\nJane,25";
console.log(parseCSV(csvData));
```
Wynik parsowania:
```
[
  ["name", "age"],
  ["John", "30"],
  ["Jane", "25"]
]
```

```Javascript
// Konwersja do CSV:
const jsonToCSV = (json) => {
  const rows = json.map(entry => Object.values(entry).join(","));
  return rows.join("\n");
};

const jsonData = [{ name: "John", age: "30" }, { name: "Jane", age: "25" }];
console.log(jsonToCSV(jsonData));
```
Wynik konwersji:
```
John,30
Jane,25
```

## Głębsze spojrzenie
CSV powstało w latach 70. Alternatywą jest JSON, który jest bardziej elastyczny. Jednak CSV jest prostsze w obsłudze i możliwe do odczytania bez specjalnych parserów. W implementacjach trzeba uwzględnić różnice w kodowaniach znaków i konwencje dotyczące separatorów, np. przecinek czy średnik.

## Zobacz również
- MDN Web Docs - praca z tekstem: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
- Specyfikacja formatu CSV: https://tools.ietf.org/html/rfc4180
- Papa Parse - potężny parser CSV dla JavaScript: https://www.papaparse.com/
