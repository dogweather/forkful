---
title:                "TypeScript: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy podczas programowania musimy operować na ciągach znaków, zwanych też stringami. Często potrzebujemy znać długość takiego ciągu, aby wykonać pewne operacje lub weryfikować poprawność danych. W tym artykule dowiesz się, jak w prosty sposób znaleźć długość stringa w języku TypeScript.

## Jak to zrobić

Pierwszą metodą jest użycie właściwości `length`, która jest dostępna dla każdego stringa w TypeScript. Przykładowy kod i wynik działania wyglądają następująco:

```TypeScript
let str: string = "To jest przykładowy string";
console.log(str.length); // Output: 25
```

Inną możliwością jest użycie funkcji `String.length()`, która również zwraca długość stringa. Przykład:

```TypeScript
let name: string = "Anna";
console.log(name.length()); // Output: 4
```

W przypadku, gdy chcemy znaleźć długość stringa złożonego z kilku wyrazów, należy użyć metody `split()` razem z funkcją `length()`. Funkcja `split()` rozdzieli string na elementy, a następnie możemy policzyć, ile elementów zawiera nowo powstała tablica. Przykład:

```TypeScript
let sentence: string = "To jest zdanie w języku TypeScript";
console.log(sentence.split(" ").length); // Output: 6
```

## Deep Dive

W języku TypeScript, podobnie jak w innych językach programowania, długość stringa jest liczona w oparciu o liczbę znaków w danym stringu. Warto jednak pamiętać, że w niektórych językach, np. chińskim czy japońskim, długość stringa może być obliczana w oparciu o znaki kanji, które składają się z kilku znaków.

W przypadku gdy chcemy policzyć długość stringa zawierającego specjalne znaki, należy pamiętać, że część znaków może być traktowana jako dwa znaki. Na przykład litera `ł` z polskiego alfabetu może być traktowana jako dwa znaki, więc przy liczeniu długości stringa zawierającego tę literę, wynik może być większy niż oczekiwany.

## Zobacz także
- [Dokumentacja języka TypeScript](https://www.typescriptlang.org/docs/)
- [Tutorial na temat manipulacji stringami w TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
- [Dyskusja na temat różnic w liczeniu długości stringa w różnych językach](https://stackoverflow.com/questions/10431964/strange-behaviour-on-the-length-of-an-accentuated-string-in-various-languages)