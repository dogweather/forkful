---
title:                "Wydobywanie podciągów"
html_title:           "C: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Czasami musisz pracować z długimi łańcuchami znaków i musisz wyodrębnić pewną część z nich. Na przykład, może chcesz wyodrębnić imię użytkownika z adresu e-mail lub numeru telefonu zapisanego w ustalonym formacie. W takich przypadkach przydatna jest funkcja wyodrębniania podłańcuchów.

## Jak to zrobić?

Aby wyodrębnić podłańcuch za pomocą języka C, musisz użyć funkcji `strncpy()` lub `strncat()` z biblioteki `string.h`. Oto przykładowy kod:

```C
#include <stdio.h>
#include <string.h>

int main() {
    // Tworzymy łańcuch ze znakami
    char str[] = "To jest przykladowy lancuch.";

    // Tworzymy tablicę, do której zapiszemy wyodrębniony podłańcuch
    char substr[10];

    // Wyodrębnienie podłańcucha z łańcucha str, zaczynając od indeksu 8 i o długości 8 znaków
    strncpy(substr, str+8, 8);

    // Dodanie znaku końca łańcucha do tablicy podłańcucha
    substr[8] = '\0';

    // Wyświetlenie wyodrębnionego podłańcucha
    printf("Wyodrebniony podlancuch to: %s", substr);
    return 0;
}
```

**Output:**

Wyodrebniony podlancuch to: przyklado

## Głębszy zanurzenie

Funkcja `strncpy()` wyodrębnia podłańcuch o określonej długości, podczas gdy `strncat()` łączy dwa łańcuchy, wyodrębniając podłańcuch z pierwszego łańcucha. Istnieje również funkcja `strnstr()` służąca do wyszukiwania podłańcuchów w łańcuchu. Podczas pracy z podłańcuchami należy uważać na poprawne ustawienie znaku końca łańcucha, inaczej wyniki mogą być niepoprawne.

## Zobacz też

- [Dokumentacja funkcji strncpy()](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Dokumentacja funkcji strncat()](https://www.tutorialspoint.com/c_standard_library/c_function_strncat.htm)
- [Dokumentacja funkcji strnstr()](https://www.tutorialspoint.com/c_standard_library/c_function_strnstr.htm)