---
title:                "Praca z formatem json"
html_title:           "Javascript: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON jest powszechnie używanym formatem danych w dzisiejszym świecie cyfrowym. Jest on wszechstronny, prosty w użyciu i można go łatwo integrować z różnymi językami programowania. Jeśli jesteś programistą, który chce nauczyć się pracy z JSON, ten artykuł jest dla Ciebie!

## Jak to zrobić

```Javascript
const person = {
  name: "Julia",
  age: 25,
  profession: "Developer",
  skills: ["JavaScript", "HTML", "CSS"],
};

// Przykład 1: Tworzenie obiektu JSON

// Przypisanie obiektu JSON do zmiennej
const personJSON = JSON.stringify(person);

console.log(personJSON);

// Output: {"name":"Julia","age":25,"profession":"Developer","skills":["JavaScript","HTML","CSS"]}

// Przykład 2: Odczytywanie obiektu JSON

// Tworzenie zmiennej z przykładowym obiektem JSON
const data = '{"name":"Adam","age":30,"profession":"Designer","skills":["Photoshop","Illustrator","InDesign"]}';

// Parsowanie JSON do obiektu
const person2 = JSON.parse(data);

console.log(person2.name); // Output: Adam
console.log(person2.age); // Output: 30
console.log(person2.profession); // Output: Designer
console.log(person2.skills); // Output: ["Photoshop","Illustrator","InDesign"]
```

## Głębszy zbiór informacji

JSON, czyli JavaScript Object Notation, jest formatem danych opartym na składni Javascript. Jest on wykorzystywany do przechowywania i przesyłania informacji pomiędzy aplikacjami i serwerami. JSON jest czytelny dla człowieka oraz łatwy do przetwarzania przez komputery, dlatego też jest on preferowanym formatem w dzisiejszych aplikacjach internetowych.

Istnieją różne metody pracy z JSON we frameworkach, takich jak Node.js czy Angular. Więcej informacji na ten temat można znaleźć w dokumentacjach tych narzędzi.

## Zobacz również

- [Dokumentacja JSON](https://www.json.org/json-pl.html)
- [Wprowadzenie do pracy z JSON w Node.js](https://docs.node.in/przyklady/praca-z-json)
- [Praca z JSON w Angular](https://angular.io/guide/http#working-with-json)