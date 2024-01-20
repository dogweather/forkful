---
title:                "Praca z json"
html_title:           "TypeScript: Praca z json"
simple_title:         "Praca z json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego? 

Artykuł ten przedstawia metodę pracy z JSON w języku TypeScript oraz wyjaśnia, dlaczego programiści decydują się na jej użycie. JSON (JavaScript Object Notation) jest to lekki format wymiany danych, który jest wygodny i prosty w użyciu. Programiści często wybierają JSON, ponieważ jest wygodnym sposobem na przechowywanie i przesyłanie danych wewnątrz aplikacji.

## Jak to zrobić?

Aby rozpocząć pracę z JSON w TypeScript, należy zaimportować odpowiedni moduł:

```TypeScript
import { JSON } from 'typescript-json';
```

Następnie można przekonwertować obiekt JavaScript na format JSON, używając metody ```stringify()```:

```TypeScript
let myObject = {
    name: "John",
    age: 30,
    address: "123 Main Street"
};

let myJSON = JSON.stringify(myObject);

console.log(myJSON); // output: {"name":"John","age":30,"address":"123 Main Street"}
```

Aby przekonwertować dane z formatu JSON na obiekt JavaScript, używa się metody ```parse()```:

```TypeScript
let myJSON = '{"name":"John","age":30,"address":"123 Main Street"}';

let myObject = JSON.parse(myJSON);

console.log(myObject.name); // output: John
```

## Głębsza analiza

Historia JSON sięga roku 2001, kiedy to Douglas Crockford wprowadził ten format jako alternatywną metodę przechowywania danych w języku JavaScript. JSON jest teraz powszechnie wykorzystywany w aplikacjach internetowych i mobilnych. Jedną z alternatyw dla JSON jest XML, jednak JSON jest znacznie lżejszy, prostszy w użyciu i szybszy w przetwarzaniu danych.

W języku TypeScript możliwe jest również wykorzystanie interfejsów w celu definiowania struktury danych w formacie JSON. Dzięki temu można uniknąć popełniania błędów w przekonwertowaniu danych z formatu JSON na obiekt JavaScript.

## Zobacz również

- [Oficjalna dokumentacja TypeScript](https://www.typescriptlang.org/docs)
- [Strona projektu JSON](https://www.json.org/)