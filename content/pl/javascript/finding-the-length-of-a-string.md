---
title:                "Javascript: Znajdowanie długości ciągu znaków"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Długość ciągu znaków jest jedną z podstawowych operacji w programowaniu. Wiele zadań, takich jak walidacja danych czy manipulacja tekstem, wymaga znajomości długości stringa. Dlatego warto poznać różne sposoby na znalezienie długości ciągu znaków w języku Javascript.

## Jak to zrobić

Istnieją trzy podstawowe sposoby na znalezienie długości stringa w języku Javascript:

1. Metoda `length`: Jest to wbudowana metoda, która zwraca liczbę znaków w danym stringu. Przykładowo:

```Javascript
let str = "Hello World";
console.log(str.length); // Output: 11
```

2. Pętla `for`: Możemy także użyć pętli `for`, aby przeiterować przez każdy znak w stringu i zliczyć je. Przykładowo:

```Javascript
let str = "Hello World";
let count = 0;
for (let i = 0; i < str.length; i++) {
    count++;
}
console.log(count); // Output: 11
```

3. Metoda `split` i `length`: Metoda `split` dzieli string na tablicę podczas gdy `length` zwraca długość tablicy, czyli liczbę znaków. Przykładowo:

```Javascript
let str = "Hello World";
console.log(str.split("").length); // Output: 11
```

## Głębszy wgląd

W przypadku metody `length` warto pamiętać, że funkcja uwzględnia wszystkie znaki, nawet spacje. Natomiast przy użyciu pętli lub metody `split`, możemy wykluczyć białe znaki za pomocą funkcji `trim()`.

Dodatkowo, metoda `length` może być również wykorzystywana do walidacji czy dany string spełnia wymaganą liczbę znaków.

## Zobacz także

- [JavaScript Strings - w3schools.com](https://www.w3schools.com/js/js_strings.asp)
- [String Length - developer.mozilla.org](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)