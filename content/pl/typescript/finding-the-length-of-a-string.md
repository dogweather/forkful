---
title:                "Wyszukiwanie długości ciągu znaków"
html_title:           "TypeScript: Wyszukiwanie długości ciągu znaków"
simple_title:         "Wyszukiwanie długości ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś może chcieć sprawdzić długość ciągu znaków? Niektóre zadania wymagają, aby programista mógł określić długość tekstu, np. dla celów walidacji lub obróbki danych. W takiej sytuacji znajomość sposobu na znalezienie długości ciągu jest niezbędna.

## Jak to zrobić

Sprawdzenie długości ciągu znaków może być wykonane w prosty sposób przy użyciu wbudowanej metody `.length`. Możemy to zobaczyć na przykładzie poniżej:

```TypeScript
let text: string = "Przykładowy tekst";
console.log(text.length);
```

Wynik powyższych działań będzie wynosił 18, ponieważ długość tekstu "Przykładowy tekst" wynosi właśnie 18 znaków.

## Głębsza analiza

Długość ciągu znaków jest obliczana na podstawie liczby znaków w tekście. W przypadku wykorzystania metody `.length` dla zmiennych typu string, jest ona automatycznie pobierana zapisana długość tekstu. Jednocześnie, jeśli zmienna jest typu number, zostanie zwrócona ilość cyfr w liczbie.

Metoda `.length` jest również użyteczna w przypadku wielu innych typów danych, takich jak tablice czy obiekty. W tym przypadku, zwracana jest ilość elementów lub właściwości w danym obiekcie. Ważne jest jednak, aby wiedzieć, że metoda `.length` nie jest dostępna dla typów danych takich jak number czy boolean.

## Zobacz także

- [Dokumentacja TypeScript: String i array methods](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string-and-array-methods)
- [MDN: String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)