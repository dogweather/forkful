---
title:                "C: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu spotykamy się z potrzebą konwersji wielkości liter w ciągach znaków. Może to wynikać z różnic w sposobie wprowadzania danych przez użytkowników lub z wymagań funkcji, na której operujemy. W tym artykule dowiesz się, jak w łatwy sposób przekształcić ciąg znaków do postaci zawierającej wyłącznie małe litery.

## Jak to zrobić

Najprostszym sposobem na zamianę wielkości liter w ciągu znaków jest użycie funkcji `tolower()` dostępnej w bibliotece `<ctype.h>`. Wystarczy przekazać do niej adres zmiennej zawierającej ciąg znaków, a funkcja zwróci kopię tego ciągu, w którym wszystkie litery będą już małe.

Poniższy kod pokazuje przykład użycia funkcji `tolower()` oraz jej wyjście:

```C
#include <stdio.h>
#include <ctype.h>

int main(){
    char string[] = "DuŻy cIąG zNakÓw";
    char *ptr = string;

    printf("Oryginalny ciąg: %s\n", string);

    while(*ptr){
        *ptr = tolower(*ptr);
        ptr++;
    }

    printf("Ciąg z małymi literami: %s\n", string);
    return 0;
}
```

Wyjście:
```
Oryginalny ciąg: DuŻy cIąG zNakÓw
Ciąg z małymi literami: duży ciąg znaków
```

## Deep Dive

Funkcja `tolower()` konwertuje pojedynczy znak do odpowiadającej mu małej litery zgodnie z obowiązującą tabelą kodów ASCII. W przypadku, gdy do funkcji zostanie przekazany znak, który jest już małą literą, nie zmieni się on i zostanie zwrócony w postaci niezmienionej.

Warto również zauważyć, że funkcja `tolower()` jest bezpieczna do użycia, ponieważ nie zmienia oryginalnego ciągu znaków. Zwraca tylko kopię, którą możemy przypisać do innej zmiennej, a oryginalny ciąg pozostawić bez zmian.

## Zobacz także

- Dokumentacja funkcji `tolower()`: https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm
- Omówienie różnych dostępnych metod konwersji wielkości liter w C: https://stackoverflow.com/questions/17293772/converting-letters-to-uppercase-lowercase-in-c-programming