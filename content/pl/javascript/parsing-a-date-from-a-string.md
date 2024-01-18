---
title:                "Analiza daty z ciągu znaków."
html_title:           "Javascript: Analiza daty z ciągu znaków."
simple_title:         "Analiza daty z ciągu znaków."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie daty z ciągu znaków jest procesem przekształcania tekstu w formę daty, którą można łatwo manipulować i wyświetlać. Programiści wykonują to zadanie, aby poprawnie interpretować daty w różnych formatach i umożliwić wygodną pracę z nimi.

## Jak to zrobić:
```Javascript
const date = new Date('2021-01-20');
console.log(date.toDateString()); // Wed Jan 20 2021
```

W powyższym przykładzie, utworzono nowy obiekt daty, przekazując ciąg znaków w odpowiednim formacie. Następnie użyto metody `toDateString()` do wyświetlenia daty w czytelnej formie.

## Wgląd:
Parsowanie daty z ciągu znaków było szczególnie ważne w czasach przed rozwinięciem języka Javascript. Wówczas, nie było wbudowanego konstruktora `Date()` i programiści musieli korzystać z niestandardowych funkcji do obsługi dat. Obecnie istnieją również inne metody parsowania dat, takie jak biblioteka Moment.js, jednak wbudowane funkcje Javascript są zazwyczaj wystarczające dla prostych zastosowań.

## Zobacz też:
- [Dokumentacja Javascript Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Dokumentacja](https://momentjs.com/docs/)
- [Parsowanie dat z ciągu znaków w Pythonie](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)