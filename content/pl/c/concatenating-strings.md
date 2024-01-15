---
title:                "Łączenie ciągów znaków"
html_title:           "C: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Jeśli jesteś programistą w języku C, bardzo prawdopodobne, że w pewnym momencie będziesz musiał złączyć dwa lub więcej napisów (stringów). Może to być potrzebne do wyświetlenia użytkownikowi czytelniejszej wiadomości lub do przekazania danych do innej funkcji. W tym artykule dowiesz się, jak w prosty sposób połączyć stringi w C.

## Jak To Zrobić?

Aby połączyć dwa stringi, należy użyć funkcji `strcat()`.  W poniższym przykładzie połączymy dwa napisy "Hello" i "World" i wyświetlimy je jako jedną wiadomość:

```C
#include<stdio.h>
#include<string.h>
int main()
{
    char string1[20] = "Hello";
    char string2[20] = "World";
    
    /* używamy funkcji strcat() do połączenia stringów */
    strcat(string1, string2);
    
    /* wyświetlamy wynik */
    printf("Połączone napisy: %s", string1);
    
    return 0;
}
```

#### Wynik:
```
Połączone napisy: HelloWorld
```

## Deep Dive

Funkcja strcat() jest zdefiniowana w bibliotece string.h i jest częścią standardu języka C. Jej prototyp wygląda następująco:

```C
char *strcat(char *string1, const char *string2)
```

Pierwszy argument (`string1`) jest napisem, do którego zostanie dołączony drugi argument (`string2`). Funkcja zwraca wskaźnik do pierwszego argumentu `string1`.

Należy zwrócić uwagę, że pierwszy argument musi mieć wystarczającą ilość miejsca, aby pomieścić zawartość drugiego argumentu wraz z null-terminatorem, w przeciwnym razie może dojść do przepełnienia bufora.

Więcej informacji na temat funkcji strcat() i innych funkcji związanych z manipulacją napisami w języku C można znaleźć w [dokumentacji](https://en.cppreference.com/w/c/string/byte/strcat).

## Zobacz Również

- [Funkcja strcpy() w języku C](https://www.programiz.com/c-programming/library-function/string.h/strcpy)
- [Porównywarka stringów w języku C](https://www.geeksforgeeks.org/compare-two-strings-in-c-3-different-ways/)
- [Operacje na stringach w C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)