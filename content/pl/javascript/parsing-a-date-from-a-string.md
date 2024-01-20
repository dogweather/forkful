---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie daty ze stringa polega na przekształceniu tekstu w obiekt daty. Programiści robią to, aby móc manipulować danymi daty i czasu oraz porównywać różne daty.

## Jak to zrobić:
Poniżej znajduje się przykład, jak można przeprowadzić parsowanie daty w JavaScript przy użyciu metody `Date.parse()`:

```Javascript 
let dateStr = "2022-03-15T21:45:00Z";
let dateObj = new Date(Date.parse(dateStr));
console.log(dateObj);
```
Gdy uruchomisz ten kod, otrzymasz coś takiego:

```Javascript
2022-03-15T21:45:00.000Z
```
Możemy również sparsować daty w innych formatach, na przykład:

```Javascript 
let dateStr2 = "15 Mar 2022 21:45:00 GMT";
let dateObj2 = new Date(Date.parse(dateStr2));
console.log(dateObj2);
```
W wyniku otrzymamy:

```Javascript
2022-03-15T21:45:00.000Z
```

## Głębsze spojrzenie 
Parsowanie daty ze stringa jest ważnym elementem manipulacji danymi daty w JavaScript, odkąd ten język został stworzony. Istniej jednak wiele technik do jego wykonania. 

Oprócz `Date.parse()`, możemy też użyć konstruktora `Date()`, który jest bardziej elastyczny, ale może prowadzić do niejednoznaczności. 

Detail implementacji `Date.parse()` może się różnić w zależności od silnika JavaScript. Zawsze warto sprawdzić specyfikację ECMAScript dla dokładnych szczegółów.

## Zobacz również
- Dokumentacja Mozilla Developer Network na temat [Date.parse()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- Wytyczne [ECMAScript](https://tc39.es/ecma262/#sec-date.parse) dla Date.parse()
- [Moment.js](https://momentjs.com/), biblioteka JavaScript do manipulacji datami. 
- [Date-fns](https://date-fns.org/), nowoczesna alternatywa dla Moment.js.