---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?
Interpolacja łańcuchów to proces wstawiania zmiennych bezpośrednio do łańcuchów. Programiści robią to dla zwiększenia czytelności i efektywności swojego kodu.

## Jak to zrobić:

```Javascript
let imie = "Jan";
console.log(`Cześć, ${imie}!`);    // Wynik: "Cześć, Jan!"
```

W powyższym kodzie, `${imie}` to przykład interpolacji łańcuchów.

```Javascript
let wiek = 25;
console.log(`Mam ${wiek} lat.`);    // Wynik: "Mam 25 lat."
```

Na powyższym przykładzie widać, że interpolacja łańcuchów jest elastyczna i może obejmować różne typy zmiennych, takie jak numer.

## Pogłębiona analiza:

1. Kontekst historyczny: Interpolacja łańcuchów została wprowadzona w ES6 / ES2015 w ramach ulepszenia składni łańcuchów, które wcześniej wymagały użycia "+" do konkatenacji łańcuchów.

2. Alternatywy: Można użyć metody `concat()` lub operatora "+". Ale interpolacja łańcuchów jest bardziej zalecana, ponieważ jest bardziej czytelna.

3. Szczegóły implementacji: Interpolowane łańcuchy są w rzeczywistości szablonami literałów klasy `Template String`. Można je używać do wstawiania wartości zmiennych, a także do wykonania wyrażeń js wewnątrz łańcucha.

## Zobacz także:

1. [MDN: Szablony literałów](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Template_literals)
2. [Interpolacja łańcuchów w Javascript](https://www.w3schools.com/js/js_string_templates.asp)