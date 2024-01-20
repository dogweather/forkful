---
title:                "Zamiana tekstu na wielkie litery"
html_title:           "Javascript: Zamiana tekstu na wielkie litery"
simple_title:         "Zamiana tekstu na wielkie litery"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zamiana pierwszej litery łańcucha na wielką, nazywana również kapitalizacją, to popularna operacja w programowaniu. Dzieje się tak, aby poprawić prezentację tekstu dla końcowego użytkownika, np. w formularzach, wiadomościach, nagłówkach i innych miejscach, gdzie to ma sens.

## Jak to zrobić:

Zamiana pierwszej litery na wielką w JavaScript jest nawykiem dostatecznie starym, by istniały różne metody. Poniżej prezentuję kod jednej z najprostszych.

```Javascript
function capitalize(str) {
    return str.charAt(0).toUpperCase() + str.slice(1);
}
console.log(capitalize('programowanie'));  // Wynik: "Programowanie"
```

Numer '0' w .charAt(0) oznacza pierwsze miejsce w łańcuchu. .toUpperCase() zamienia tę literę na wielką. .slice(1) dodaje do niej resztę łańcucha.

## Głębokie zanurzenie

Zanim zaczniemy omawiać szczegóły, warto zauważyć, że kapitalizacja znaków jest znacznie starsza od JavaScriptu. Programiści odkryli jej wartość już dawno temu i zaimplementowali ją w większości języków programowania.

Co do alternatywnych metod, wydajniejszą, ale bardziej skomplikowaną jest użycie wyrażeń regularnych.

```Javascript
function capitalize(str) {
    return str.replace(/^\w/, c => c.toUpperCase());
}
```
Wyrażenie regularne /^\w/ pasuje do pierwszego znaku alfanumerycznego. Funkcja strzałkowa c => c.toUpperCase() "podnosi" pasujący znak. Ta metoda jest zwłaszcza użyteczna, gdy tekst zawiera wiele zdań lub akapitów do kapitalizacji.

## Zobacz też

Dla dalszego czytania i nauki polecam:

- Mozilla Developer Network (MDN): String.prototype.charAt() (https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- MDN: String.prototype.replace() (https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- JavaScript.info: Wyrażenia Regularne (https://javascript.info/regular-expressions)
- StackOverflow: Jak zmienić pierwszą literę na dużą w JavaScript? (https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript)