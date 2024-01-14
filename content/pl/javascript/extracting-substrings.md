---
title:                "Javascript: Wycinanie podciągów"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Czasami w programowaniu konieczne jest wydobycie fragmentu z tekstu lub ciągu znaków. Może to być potrzebne do porównania, edycji lub wykorzystania w inny sposób. W takich sytuacjach użyteczna jest operacja zwana wydobyciem podciągu (ang. substring).

## Jak to zrobić?

Aby wydobyć podciąg za pomocą JavaScript, musimy użyć wbudowanej funkcji `substring()`. Podajemy dwa parametry - początek i koniec wybranego fragmentu. Na przykład, jeśli chcemy wydobycie podciągu z drugiego do czwartego znaku z ciągu "Hello World!", nasz kod może wyglądać tak:

```Javascript
let text = "Hello World!";
let substring = text.substring(2, 5);
console.log(substring); // wynik: "llo"
```

Możemy również podać tylko jeden parametr - początek. Wtedy zwrócony zostanie podciąg od tego miejsca do końca oryginalnego tekstu. Przykład:

```Javascript
let text = "Hello World!";
let substring = text.substring(5);
console.log(substring); // wynik: " World!"
```

Pamiętajmy, że indeksowanie znaków w JavaScript zaczyna się od 0, więc pierwszy znak ma indeks 0.

## Deep Dive

Funkcja `substring()` jest często używana do manipulacji i edycji tekstów w JavaScript. Jest to jednak tylko jedna z wielu dostępnych metod. Innymi przydatnymi funkcjami są `slice()`, `substr()` czy `replace()`. W zależności od naszych potrzeb, możemy wybrać odpowiednią do danego zadania funkcję.

Warto również wiedzieć, że `substring()` działa tylko na typie danych "string", dlatego jeśli chcemy wykonać operację na innym typie, np. liczbie, musimy najpierw przekonwertować go na string za pomocą funkcji `toString()`.

## Zobacz także

1. Dokumentacja wbudowanej funkcji `substring()` w JavaScript: https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/substring
2. Porównanie różnych funkcji do manipulacji stringami w JavaScript: https://www.w3schools.com/js/js_string_methods.asp
3. Przykłady zastosowań wydobycia podciągu w praktyce: https://www.geeksforgeeks.org/extracting-substrings-from-a-string-in-javascript/