---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Długość łańcucha w TypeScript: Co, Dlaczego i Jak?

## Co i Dlaczego?
Znajomość długości łańcucha, czyli ilości znaków w ciągu, jest kluczowym elementem dla wielu operacji w programowaniu. Pozwala na kontrolowanie wprowadzonych danych, przycinanie długich tekstu czy tworzenie pętli, które przeszukują poszczególne znaki łańcucha.

## Jak to zrobić:
W TypeScript, aby znaleźć długość łańcucha, korzystamy z właściwości `length`. Zobacz przykład poniżej:

```TypeScript
let napis: string = "Dzień dobry, Polsko!";
let długośćNapisu: number = napis.length;
console.log(długośćNapisu); // Wydrukuje: 20
```
Ten kod wyznacza długość łańcucha "Dzień dobry, Polsko!", a następnie wydrukuje rezultat: 20.

## Deep Dive
Ale skąd właściwość 'length' wzięła się w JavaScript / TypeScript?

Właściwość 'length' pochodzi z JavaScript, z którym TypeScript jest ściśle powiązany. Ta właściwość od zawsze istniała w JavaScript dla manipulowania ciągami znaków i tablicami.

Nie zawsze musisz korzystać z metody `length` do znalezienia długości łańcucha. Przykładowo, w Pythonie używamy funkcji `len()`. Ale TypeScript, jak JavaScript, preferuje używanie właściwości `length`.

Ponadto, warto wiedzieć, że liczba wynikowa uwzględnia również spacje i znaki specjalne.

## Zobacz także:
1. [TypeScript - String length Property - Tutorialspoint](https://www.tutorialspoint.com/typescript/typescript_string_length.htm)
2. [JavaScript String length Property - W3Schools](https://www.w3schools.com/jsref/jsref_length_string.asp)
3. [Mozilla Developer Network (MDN) - String.length ](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)