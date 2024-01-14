---
title:                "C: Znajdowanie długości ciągu znaków"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego znalezienie długości ciągu jest ważne w C?

Znalezienie długości ciągu (lub inaczej mówiąc, liczby znaków znajdujących się w danym ciągu) jest podstawową operacją w wielu językach programowania, w tym w C. Jest to często używane podczas manipulacji ciągami tekstu, takimi jak wyświetlanie, porównywanie lub kopiowanie. W tym artykule dowiesz się, jak wykonać to zadanie w C.

## Jak to zrobić?

Najprostszym sposobem na znalezienie długości ciągu jest użycie funkcji **strlen()** z biblioteki string.h. Przyjmuje ona jako argument ciąg znaków i zwraca liczbę znaków w tym ciągu. Przykładowy kod wyglądałby następująco:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "Witaj świecie!";
    int len = strlen(str);

    printf("Długość ciągu '%s' to %d", str, len);
}
```

**Output:**

```
Długość ciągu 'Witaj świecie!' to 14
```

Możesz również ręcznie policzyć długość ciągu, korzystając z pętli **while** i zwiększając licznik z każdym kolejnym znakiem, dopóki nie napotkasz znaku końca ciągu **\0**. Przykładowy kod wyglądałby tak:

```C
#include <stdio.h>

int main()
{
    char str[] = "Cześć!";
    int i = 0;
    while (str[i] != '\0')
    {
        i++;
    }

    printf("Długość ciągu '%s' to %d", str, i);
}
```

**Output:**

```
Długość ciągu 'Cześć!' to 6
```

## Głębszy przegląd

Znajomość długości ciągu jest bardzo ważna przy manipulacji napisami w C. Musisz pamiętać, że dla funkcji **strlen()**, sama długość znaków ma znaczenie, a nie długość tablicy przechowującej ciąg. Na przykład, jeśli stworzysz tablicę o rozmiarze 10 znaków i zainicjujesz ją jako "Cześć!", funkcja **strlen()** zwróci 6, ponieważ są to tylko znaki widoczne, reszta tablicy jest uzupełniona zerami. 

Pamiętaj również, że znak końca ciągu **\0** jest liczbą 0, więc jeśli zmodyfikujesz go na inną liczbę, funkcja **strlen()** zwróci błędne wyniki. 

## Zobacz także

- [Podstawowe operacje na napisach w C](https://www.programiz.com/c-programming/c-strings)
- [Dokumentacja funkcji strlen()](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [Inne funkcje z biblioteki string.h](https://www.tutorialspoint.com/c_standard_library/string_h.htm)