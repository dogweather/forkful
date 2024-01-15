---
title:                "Łączenie ciągów znaków"
html_title:           "Javascript: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Konkatenacja, czyli łączenie ciągów znaków, jest powszechnie stosowaną operacją w programowaniu. Pozwala ona na tworzenie dynamicznych i spersonalizowanych wiadomości, np. w aplikacjach internetowych czy grach komputerowych. Dzięki niej można również wyświetlać dane użytkowników w czytelnej i estetycznej formie.

## Jak to zrobić

Aby połączyć dwa ciągi znaków, używamy operatora plus "+". Możemy również wykorzystać metodę `concat()`, która działa na tablicach i łączy elementy w jeden string. Przykładowy kod wyglądałby następująco:

```Javascript
let imie = "Jan";
let nazwisko = "Kowalski";

let pelneImie = imie + " " + nazwisko;
//wynik: "Jan Kowalski"

let dane = [imie, nazwisko];
let pelneDane = dane.concat(["28 lat"]);
//wynik: ["Jan", "Kowalski", "28 lat"]
```

Jeśli chcemy dodatkowo sformatować nasze stringi, możemy wykorzystać metody takie jak `slice()` czy `trim()`, a także wyrażenia szablonowe (ang. template literals), które pozwalają na wygodniejsze łączenie zmiennych z tekstem. 

## Głębsza analiza

Podczas konkatenacji zachodzi konwersja typów, dlatego ważne jest, aby pamiętać o typach danych podczas łączenia stringów z innymi typami. Ponadto, jeśli musimy połączyć wiele ciągów znaków, warto skorzystać z metody `join()`, która jest bardziej wydajna niż kolejne użycia operatora plus.

## Zobacz również

- [Mozilla Developer Network: Concatenate strings](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [W3Schools: JavaScript String Concatenation](https://www.w3schools.com/js/js_string_concat.asp)
- [JavaScript.info: String Concatenation](https://javascript.info/string#concatenation)