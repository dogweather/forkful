---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "C: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja ciągu znaków na małe litery jest niezbędna w wielu sytuacjach w programowaniu. Może to pomóc w porównywaniu tekstów, filtrowaniu danych i wielu innych zastosowaniach.

## Jak to zrobić

Konwersja ciągu znaków na małe litery jest bardzo prosta w języku C. Do jej wykonania należy użyć funkcji `tolower()` z biblioteki `ctype.h`. Poniżej znajduje się przykładowy kod:

```C
#include <stdio.h>
#include <ctype.h>

int main(void)
{
    char string[] = "PRZYKŁADOWY TEKST";
    int i = 0;

    while (string[i])
    {
        putchar(tolower(string[i]));
        i++;
    }

    return 0;
}
```

Wynikiem działania programu będzie: `przykładowy tekst`.

## Głębszy wywiad

Funkcja `tolower()` konwertuje pojedynczy znak na małą literę. Jest ona zdefiniowana w standardowej bibliotece `ctype.h` i przyjmuje jeden parametr typu `int`, który jest kodem ASCII znaku. Jeśli podamy jej znak, który jest już małą literą, zostanie on zwrócony bez zmiany. Jeśli natomiast podamy znak, który jest wielką literą, zostanie on przekonwertowany na małą literę.

Warto również pamiętać, że funkcja `tolower()` może działać tylko na pojedynczych znakach. Dlatego, jeśli chcemy skonwertować cały ciąg znaków, musimy użyć jej w pętli, jak w przykładzie powyżej.

## Zobacz również

- Dokumentacja funkcji `tolower()` w języku C: [https://en.cppreference.com/w/c/string/byte/tolower](https://en.cppreference.com/w/c/string/byte/tolower)

- Przykładowe zadania z wykorzystaniem konwersji na małe litery: [https://www.w3resource.com/c-programming-exercises/](https://www.w3resource.com/c-programming-exercises/)

- Rozbudowane informacje o bibliotece `ctype.h`: [https://www.tutorialspoint.com/c_standard_library/ctype_h.htm](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)