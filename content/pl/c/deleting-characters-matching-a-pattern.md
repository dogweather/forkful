---
title:                "Usuwanie znaków pasujących do wzorca"
date:                  2024-01-20T17:41:44.512969-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Usuwanie znaków według wzorca to filtracja danych, pozbywamy się tym niepotrzebnych elementów. Programiści często to robią, by oczyścić dane wejściowe, przyspieszyć przetwarzanie lub usunąć zbędne informacje.

## How to: (Jak to zrobić:)
```c
#include <stdio.h>
#include <string.h>

void delete_chars(char *str, const char *pattern) {
    char *src = str, *dst = str;
    while(*src) {
        const char *p = pattern;
        while (*p && *p != *src) p++;
        if (!*p) *dst++ = *src;
        src++;
    }
    *dst = '\0';
}

int main() {
    char text[] = "Hello, World! 123";
    delete_chars(text, " !123");
    printf("Result: %s\n", text); // Wypisuje "Hello,World"
    return 0;
}
```
Sample output (Przykładowe wyjście):
```
Result: Hello,World
```

## Deep Dive (Dogłębna analiza):
Usuwanie znaków według wzorca to stara sztuczka. Zanim pojawiły się wyrażenia regularne, programiści musieli radzić sobie z tym sami – używając własnych funkcji. 

Alternatywa do zaprezentowanego podejścia to wyrażenia regularne, ale potrzebują one dodatkowych bibliotek jak `<regex.h>` w standardzie POSIX. Ta funkcja (`delete_chars`) jest przykładem prostego podejścia, gdzie iterujemy po każdym znaku i sprawdzamy, czy należy go zachować.

Szczegółowo, porównujemy każdy znak ze wzorcem. Jeśli się nie zgadza, przenosimy go do nowej pozycji. Na końcu donosimy o zakresie przetworzonego stringa dodając znak końca stringa (`\0`).

## See Also (Zobacz też):
- [C String Library Functions](http://www.cplusplus.com/reference/cstring/)
- [POSIX Regular Expressions](https://pubs.opengroup.org/onlinepubs/7908799/xsh/regex.h.html)
- [String Manipulation in C](https://en.wikibooks.org/wiki/C_Programming/String_manipulation)
