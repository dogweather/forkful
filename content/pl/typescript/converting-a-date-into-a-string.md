---
title:                "TypeScript: Konwersja daty na ciąg znaków"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem w programowaniu musimy przekształcić datę na postać tekstową, na przykład, jeśli chcemy wyświetlić ją użytkownikowi lub zapisać w pliku. W tym artykule dowiesz się, jak to zrobić w języku TypeScript.

## Jak to zrobić

```TypeScript
const date = new Date(); // tworzymy nowy obiekt daty
const dateString = date.toDateString(); // metoda toDateString() zwraca datę w formie tekstowej
console.log(dateString); // wyświetli "Mon Sep 27 2021"
```

W powyższym przykładzie użyliśmy metody `toDateString()` na obiekcie daty, aby przekonwertować ją na string. Możemy także wykorzystać inne metody, takie jak `toLocaleDateString()` lub `toISOString()`, aby uzyskać tekstową reprezentację daty w różnych formatach.

## Deep Dive

Podczas konwertowania daty na string warto wiedzieć, że istnieje wiele różnych formatów daty i godziny, w zależności od lokalizacji czy preferencji użytkownika. Dlatego ważne jest, aby wybrać odpowiednią metodę dla potrzeb naszej aplikacji. Metody `toDateString()` i `toLocaleDateString()` przyjmują opcjonalne argumenty określające język i strefę czasową, zaś metoda `toISOString()` zwraca zawsze datę i godzinę w formacie UTC.

## Zobacz także

- [Dokumentacja dla metody toDateString()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString)
- [Dokumentacja dla metody toLocaleDateString()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [Dokumentacja dla metody toISOString()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString)