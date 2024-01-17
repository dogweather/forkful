---
title:                "Zmiana wielkości liter w ciągu znaków"
html_title:           "C: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zacznijmy od podstaw - co to właściwie znaczy "kapitalizowanie" ciągu znaków? W prostych słowach, oznacza to zamienienie wszystkich liter w wyrazie na wielkie litery. Dlaczego programiści robią to? Przede wszystkim po to, aby ujednolicić dane wejściowe i uprościć porównywanie i przetwarzanie danych.

## Jak to zrobić:

```C
#include <stdio.h>
#include <string.h>

void capitalize_string(char* str)
{
    int i;
    for (i = 0; i < strlen(str); i++)
    {
        if (str[i] >= 'a' && str[i] <= 'z')
        {
            str[i] -= 32; // ASCII: 'a' = 97, 'A' = 65, dlatego '- 32'
        }
    }
}

int main()
{
    char word[] = "programowanie";
    capitalize_string(word);
    printf("%s\n", word);

    return 0;
}
```
Wyjście: "PROGRAMOWANIE"

## Głębsza analiza:

Jeśli zawsze używaliśmy tylko języka polskiego, to nie zdajemy sobie sprawy, że w innych językach istnieją typy liter, takie jak wielkie i małe litery. W przypadku języka C, różnica ta jest szczególnie ważna - wszystkie literki składają się z kombinacji zer i jedynek w postaci kodu ASCII. Dlatego, aby "kapitalizować" ciąg znaków, musimy zmniejszyć wartość odpowiadającą małej literze o 32, aby otrzymać kod odpowiadający jej odpowiednikowi w postaci wielkiej litery.

Alternatywnym sposobem na kapitalizowanie ciągu znaków jest użycie funkcji ```toupper()``` lub biblioteki <ctype.h>. Jednakże, aby uzyskać lepszą kontrolę nad procesem, warto napisać własną funkcję, jak w przykładzie powyżej.

## Zobacz też:

- [Funkcja toupper() w języku C] (https://www.programiz.com/c-programming/library-function/ctype.h/toupper)
- [Kod ASCII w języku C] (https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_71/rtref/asciicodes.htm)