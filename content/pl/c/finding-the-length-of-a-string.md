---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "C: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania kodu w języku C mamy do czynienia z wykorzystywaniem ciągów znaków. Większość operacji na tych ciągach wymaga określenia ich długości, co jest niezbędne do poprawnego wykonywania operacji. Dlatego warto wiedzieć, jak znaleźć długość ciągu znaków w języku C.

## Jak to zrobić

Aby znaleźć długość ciągu znaków w języku C, możemy wykorzystać funkcję `strlen()`, która jest dostępna w bibliotece standardowej `string.h`. Należy jednak pamiętać, że ta funkcja nie uwzględnia znaku końca ciągu (`'\0'`) i zwraca liczbę znaków przed tym znakiem. Spójrzmy na prosty przykład:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello World!";
    int length = strlen(str);

    printf("Długość ciągu znaków '%s' to %d\n", str, length);

    return 0;
}
```

**Wyjście:**

```
Długość ciągu znaków 'Hello World!' to 12
```

Mamy tu zdefiniowaną zmienną `str`, która przechowuje ciąg znaków "Hello World!". Następnie wykorzystujemy funkcję `strlen()` i przypisujemy jej wynik do zmiennej `length`. Wypisujemy te wartości na ekranie i dostajemy oczekiwany wynik.

## Wyróżnijmy szczegóły

Chociaż funkcja `strlen()` jest bardzo przydatna, to warto wiedzieć, że operuje ona na tablicach typu `char`. Oznacza to, że jeśli będziemy jej używać do innych typów danych, takich jak `int` czy `float`, to nie otrzymamy poprawnego wyniku. W takiej sytuacji najlepiej użyć funkcji `sizeof()`, która zwróci nam liczbę bajtów, a nie znaków w ciągu.

Pamiętajmy także, że funkcja `strlen()` może zwracać nieprawidłowy wynik, jeśli przekazany jej ciąg znaków nie ma znaku końca (`'\0'`). Jest to przydatne, jeśli chcemy wykorzystać bardziej wyrafinowane metody manipulacji ciągami znaków.

## Zobacz także

- [Dokumentacja funkcji strlen()](https://www.cplusplus.com/reference/cstring/strlen/)
- [Wykorzystanie funkcji strlen() w praktyce](https://www.geeksforgeeks.org/strlen-function-in-c/)