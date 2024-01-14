---
title:    "C: Znajdowanie długości ciągu znaków"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego?

Znajdywanie długości łańcucha znaków jest niezbędnym elementem w wielu projektach programistycznych. Wiele funkcji i operacji wykorzystuje długość łańcucha do prawidłowego działania. Dlatego jest to ważna umiejętność do opanowania dla każdego programisty.

## Jak to zrobić?

Najprostszym sposobem na znalezienie długości łańcucha jest użycie funkcji `strlen()`. Wywołanie tej funkcji na danym łańcuchu zwraca jego długość jako wartość całkowitą. Przykładowy kod wyglądałby następująco:

```C
char str[] = "Cześć!";
int length = strlen(str);
printf("Długość łańcucha: %d", length);
```

W powyższym przykładzie, wartość `length` zostałaby ustawiona na `6`, ponieważ tyle znaków znajduje się wewnątrz łańcucha "Cześć!".

## Zagłębienie

Funkcja `strlen()` iteruje przez każdy pojedynczy znak łańcucha aż do znaku `\0`, który oznacza koniec łańcucha. Dlatego też funkcja ta nie liczy ostatniego znaku, ponieważ jest on tym znakiem kontrolnym.

Warto zwrócić uwagę, że funkcja `strlen()` jest dostępna w bibliotece `string.h`, dlatego należy ją dołączyć na początku programu za pomocą dyrektywy `#include`.

## Zobacz również

- [Dokumentacja funkcji `strlen()`](https://www.programiz.com/c-programming/library-function/string.h/strlen)
- [10 przykładów użycia funkcji `strlen()`](https://overiq.com/c-programming-101/the-strlen-function-in-c/)
- [Przewodnik po łańcuchach znaków w C](https://www.geeksforgeeks.org/strings-in-c-2/)