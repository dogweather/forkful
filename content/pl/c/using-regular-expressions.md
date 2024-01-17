---
title:                "Używanie wyrażeń regularnych"
html_title:           "C: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co & Po co?:

Regular expressions są to wyrażenia wykorzystywane w języku programowania C do wyszukiwania i manipulowania tekstem. Umożliwiają one szybsze i bardziej precyzyjne odnajdowanie wzorców w tekście, co jest szczególnie przydatne w przypadku przetwarzania dużych ilości danych.

Programiści często korzystają z wyrażeń regularnych w celu weryfikacji poprawności wprowadzonego tekstu lub wyciągania określonych informacji z tekstu wejściowego. Jest to także popularne narzędzie w automatyzacji procesów, np. w parsowaniu logów czy zamianie formatów danych.

## Jak to zrobić:

Fragmenty kodu poniżej ilustrują podstawowe przykłady użycia wyrażeń regularnych w języku C.

```
#include <stdio.h>
#include <string.h>
#include <regex.h>

int main()
{
    // Przykład 1: Wyszukiwanie słowa "regular" w tekście
    char *text = "Regular expressions are awesome!";
    char *regex = "regular";

    // Kompilacja wyrażenia regularnego
    regex_t regex_compiled; 
    regcomp(&regex_compiled, regex, 0);

    // Wyszukiwanie wzorca w tekście
    if (regexec(&regex_compiled, text, 0, NULL, 0) == 0)
    {
        printf("Znaleziono dopasowanie!");
    }
    else
    {
        printf("Dopasowanie nie znalezione.");
    }

    // Zwolnienie pamięci
    regfree(&regex_compiled);

    // Przykład 2: Zamiana tekstu
    char *quote = "You are confined only by the walls you build yourself.";
    regex = "walls";
    char *replacement = "limitations";

    // Kompilacja wyrażenia regularnego z flagą REG_EXTENDED
    // Pozwala na użycie wyrażeń regularnych w drugim argumencie funkcji regexec
    regcomp(&regex_compiled, regex, REG_EXTENDED);

    // Zamiana wzorca na podaną wartość
    char *formatted = regreplace(text, &regex_compiled, replacement);

    // Wyświetlenie wyniku
    printf("%s", formatted);

    // Zwolnienie pamięci
    regfree(&regex_compiled);

    return 0;
}

## Głębszy zanurzenie:

Wyrażenia regularne powstały w latach 50. wraz z pojawieniem się pierwszych języków do przetwarzania tekstu. Współcześnie istnieje wiele alternatywnych metod przetwarzania i manipulacji tekstu, np. funkcje string z biblioteki standardowej C. Jednak wyrażenia regularne pozostają popularnym wyborem ze względu na swoją potężną funkcjonalność i wszechstronność.

Pod maską, wyrażenia regularne są kompilowane do automatów skończonych i wykorzystują algorytm Thompsona w celu wyszukiwania wzorców w tekście. Jest to szybka i wydajna metoda, pozwalająca na przetwarzanie dużych ilości danych w krótkim czasie.

## Zobacz także:

- [Dokumentacja biblioteki regex.h w języku C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Przydatny kalkulator wyrażeń regularnych online](https://regex101.com/)