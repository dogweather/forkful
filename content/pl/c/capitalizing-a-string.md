---
title:                "Udostępnianie wartości w ciągach znakowych"
html_title:           "C: Udostępnianie wartości w ciągach znakowych"
simple_title:         "Udostępnianie wartości w ciągach znakowych"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiałeś się kiedyś, jak zmieniać wielkość liter w ciągu znaków w języku C? Może masz aplikację, która wymaga wprowadzenia danych tylko w dużych literach lub chcesz, aby tekst w Twoim programie wyglądał estetycznie. Niezależnie od tego, jest wiele sytuacji, w których może być potrzebne dokonanie zmiany wielkości liter. W tym artykule dowiesz się, jak to zrobić w prosty sposób.

## Jak to zrobić

Aby zmienić wielkość liter w ciągu znaków w języku C, musisz wykorzystać funkcję `toupper()` znajdującą się w bibliotece `ctype.h`. Ta funkcja jest odpowiedzialna za zamianę pojedynczego znaku na jego odpowiednik w dużych literach. Przykładowy kod wykorzystujący tę funkcję wygląda następująco:

```
#include <stdio.h>
#include <ctype.h>

int main()
{
    char str[] = "hello world";
    int i;

    for (i = 0; str[i] != '\0'; i++)
    {
        printf("%c", toupper(str[i])); // zamienia literę na dużą i wyświetla
    }

    return 0;
}
```

Powyższy program wypisze na ekranie ciąg znaków "HELLO WORLD", zamieniając każdą literę na dużą. Aby zmienić wielkość liter w innym ciągu znaków, wystarczy zmienić wartość zmiennej `str`.

## Deep Dive

W języku C istnieje również funkcja `tolower()`, która służy do zamiany liter na małe. Aby uzyskać pełen dostęp do wszystkich funkcji z biblioteki `ctype.h`, należy dołączyć stałą `__USE_MISC` na początku programu.

Zmiana wielkości liter w języku C może się również przydać podczas porównywania ciągów znaków. Ponieważ litery różnych wielkości są traktowane jako różne, jeśli chcesz dokonać porównania bez uwzględniania wielkości liter, musisz najpierw zmienić wszystkie litery na jedną wielkość.

Możesz także napisać własną funkcję do zmiany wielkości liter, np. zamieniając małe litery na duże i odwrotnie. Takie podejście pozwala na większą kontrolę i dostosowanie do własnych potrzeb.

## Zobacz także

- Oficjalna dokumentacja funkcji `toupper()` i `tolower()`: https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm
- Przykładowe kody do zmiany wielkości liter w języku C: https://www.programiz.com/c-programming/examples/change-case-string