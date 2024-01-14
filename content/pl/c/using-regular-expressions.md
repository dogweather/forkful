---
title:                "C: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego
RegEx to skrótowiec od "wyrażeń regularnych" i jest jednym z najpotężniejszych narzędzi w programowaniu. Pozwala na szybkie i skuteczne wyszukiwanie oraz manipulację tekstem. W tym artykule dowiesz się, dlaczego warto nauczyć się korzystać z wyrażeń regularnych w języku C.

## Jak To Zrobić
Aby używać wyrażeń regularnych w języku C, musimy najpierw zaimportować bibliotekę <regex.h>. Następnie możemy użyć funkcji regex_match do porównania przekazanego tekstu z wyrażeniem regularnym. Poniżej przedstawiamy przykładowy kod oraz przewidywane wyniki:

```C
// Przykładowy kod wykorzystujący wyrażenia regularne
#include <stdio.h>
#include <regex.h>

int main()
{
    regex_t regex;
    int result;
    char text[] = "Hello world";

    result = regcomp(&regex, "Hell.", 0); // Wyrażenie regularne "Hell." dopasowuje "Hello"
    if (result == 0) // Jeśli wynik jest równy 0, to wyrażenie zostało prawidłowo skompilowane
    {
        result = regexec(&regex, text, 0, NULL, 0); // Sprawdzamy dopasowanie tekstu "Hello world"
        if (result == 0) // Jeśli wynik jest równy 0, to wyrażenie jest dopasowane do tekstu
        {
            printf("Regular expression matched!");
        }
        else
        {
            printf("Regular expression not matched.");
        }
    }
    else
    {
        printf("Regular expression compilation failed.");
    }

    regfree(&regex); // Zwalniamy utworzoną strukturę

    return 0;
}
```

Oczekiwany wynik:
> Regular expression matched!

## Głębsze Wniknięcie
Wyrażenia regularne to nie tylko prosty sposób na porównywanie tekstu. Pozwalają także na wykonywanie zaawansowanych operacji na tekście, takich jak wyszukiwanie, zastępowanie, dzielenie oraz sprawdzanie czy dany tekst spełnia określone warunki. Dzięki temu wyrażenia regularne są niezastąpione przy przetwarzaniu dużych plików tekstowych, tworzeniu skryptów oraz walidacji danych wejściowych.

Ponadto, wyrażenia regularne są niezależne od języka i wykorzystywane w wielu innych narzędziach, takich jak edytory tekstu czy bazy danych. Dlatego nauka ich w języku C jest przydatna nie tylko w kontekście programowania, ale także przy pracy z różnymi systemami oraz aplikacjami.

## Zobacz Również
* [Dokumentacja języka C](https://devdocs.io/c/)
* [Podstawy wyrażeń regularnych w języku C](https://www.zentut.com/c-tutorial/c-regular-expression/)
* [Poradnik wyrażeń regularnych w języku C](https://www.regular-expressions.info/tutorialcnt.html)