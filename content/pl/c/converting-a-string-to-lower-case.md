---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co to i dlaczego? 
Konwersja ciągu do małych liter w C polega na zmianie wszystkich dużych liter na małe. Często programiści stosują to, aby normalizować dane wejściowe, umożliwić porównywanie ciągów niezależnie od wielkości liter oraz poprawić estetykę i czytelność danych.

## Jak to zrobić:
Użyjemy funkcji tolower() dostępnej w bibliotece ctype.h, aby przekształcić ciąg do małych liter. Oto przykładowy kod:

```C 
#include <stdio.h>
#include <ctype.h>
#include <string.h>

void convertToLower(char *str)
{
    for(int i = 0; str[i]; i++)
    {
        str[i] = tolower(str[i]);
    }
}

int main()
{
    char str[] = "Hello, World!";
    convertToLower(str);
    printf("%s\n", str); // "hello, world!"

    return 0;
}
```

## Deep Dive:
Historia: Funkcja tolower() znana jest od bardzo długiego czasu i jest często używana w programowaniu wsadowym czy programowaniu systemowym. Przyjęło się, że jest to najprostszy i najefektywniejszy sposób konwertowania ciągów na małe litery w C.

Alternatywy: Możesz napisać własną funkcję, która będzie przekształcać duże litery na małe, ale to jest bardziej pracochłonne i nie zawsze efektywne.

Szczegóły implementacji: Funkcja tolower() przekształca dane na niskich poziomach, które są niezależne od lokalizacji danego systemu, co czyni ją uniwersalnie skuteczną.

## Zobacz także:
1. [Dokumentacja Funkcji tolower()](https://www.cplusplus.com/reference/cctype/tolower/)
3. [Porównywanie ciągów C](https://www.tutorialspoint.com/c_standard_library/c_function_strcmp.htm)