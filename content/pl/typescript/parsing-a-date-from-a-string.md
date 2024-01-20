---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Przetwarzanie daty z łańcucha znaków w TypeScript
## Co i dlaczego?
Rozpoznawanie daty z tekstu to proces, w którym ściągamy zwhiskytowaną datę zawartą w stringu. Robimy to po to, żeby móc pracować ze zrozumiałą dla komputera formą daty i manipulować nią na wiele sposóbów.

## Jak to zrobić:
Podstawowy przykład jak przełożyć łańcuch na datę wygląda tak:
```TypeScript
let mojaData = new Date("2021-07-13T10:20:30Z");
console.log(mojaData);
```
Output:
```TypeScript
2021-07-13T10:20:30.000Z
```
Uważaj na format daty: "YYYY-MM-DDTHH:MM:SSZ" to tzw. format ISO, który jest odpowiednio przetwarzany przez JavaScript i TypeScript.

## Więcej szczegółów
1. **Historia:** JavaScript wcześnie nie obsługiwał formatu ISO dla dat. Do momentu standardu ECMAScript 5 (ES5), próba rozwinięcia daty z takiego stringa kończyła się błędem. TypeScript, jako nadzestaw JavaScript, dziedziczy tę funkcję i obsługuje możliwość przetwarzania daty z ISO string.

2. **Alternatywy:** Można też użyć bibliotek takich jak moment.js lub date-fns do łatwiejszego manipulowania datami. 

3. **Jak to działa:** Gdy używasz `new Date(dateString)`, tworzysz nowy obiekt daty, analizując podane łańcuch według ustalonego formatu.

## Zobacz również
1. [Dokumentacja MDN na temat obiektów daty](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date)
2. [Przetwarzanie daty w JavaScript](https://flaviocopes.com/javascript-dates/)
3. [Biblioteka moment.js](https://momentjs.com/)
4. [Biblioteka date-fns](https://date-fns.org/) 

Zapraszam do próbowania i bawienia się datami.