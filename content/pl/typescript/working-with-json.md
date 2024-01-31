---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, czyli JavaScript Object Notation, to format danych, który ułatwia wymianę i przechowywanie informacji. Programiści używają go z powodu jego prostoty, czytelności i wszechobecności w technologiach webowych.

## How to:
```TypeScript
// Definicja typu dla użytkownika
type User = {
  id: number;
  name: string;
};

// Przykładowy obiekt użytkownika
const user: User = {
  id: 1,
  name: "Jan Kowalski"
};

// Konwersja obiektu do JSON
const userJson = JSON.stringify(user);
console.log(userJson);  // Output: '{"id":1,"name":"Jan Kowalski"}'

// Konwersja z JSON do obiektu
const userParsed = JSON.parse(userJson) as User;
console.log(userParsed); // Output: { id: 1, name: 'Jan Kowalski' }
```

## Deep Dive
JSON został stworzony na początku lat 2000 jako alternatywa dla XML. Mimo że oba służą do wymiany danych, JSON zyskał popularność ze względu na mniejszy rozmiar i łatwość użycia w JavaScript. Alternatywami dla JSON-a mogą być YAML czy BSON, ale żaden z nich nie zdobył tak dużej popularności. Przy implementacji warto pamiętać o bezpiecznym parsowaniu JSON, aby uniknąć problemów związanych z niepoprawnymi danymi.

## See Also
- [TypeScript Handbook - Type Checking JavaScript Files](https://www.typescriptlang.org/docs/handbook/type-checking-javascript-files.html)
- [ECMA-404 The JSON Data Interchange Standard](https://www.ecma-international.org/publications-and-standards/standards/ecma-404/)
