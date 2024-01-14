---
title:                "C: Użycie wyrażeń regularnych"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego warto używać wyrażeń regularnych w C?

Wyrażenie regularne jest potężnym narzędziem do manipulowania danymi i tekstami w języku C. Użycie ich może znacząco ułatwić i przyspieszyć proces programowania.

## Jak używać wyrażeń regularnych w C?

Użycie wyrażeń regularnych w C jest bardzo proste. Wystarczy zaimportować bibliotekę `regex.h`, aby móc używać funkcji i wyrażeń regularnych. Poniżej znajdują się przykładowe kody i ich wyjście.

```C
#include <stdio.h>
#include <regex.h>

int main() {

    // Przykładowy tekst do wyszukania
    char *tekst = "Witaj w świecie C programming";

    // Utworzenie obiektu wyrażenia regularnego
    regex_t wyrazenie;

    // Kompilacja wyrażenia regularnego
    int result = regcomp(&wyrazenie, "C programming", 0);

    // Wyszukanie wystąpienia wyrażenia w tekście
    result = regexec(&wyrazenie, tekst, 0, NULL, 0);

    // Sprawdzenie czy wystąpiło dopasowanie
    if (result == 0) {
        printf("Wyrażenie zostało dopasowane w tekście!");
    } else {
        printf("Wyrażenie nie zostało dopasowane w tekście!");
    }

    // Zwalnianie pamięci
    regfree(&wyrazenie);

    return 0;
}
```

Wyjście:
```
Wyrażenie zostało dopasowane w tekście!
```

## Głębszy wgląd w użycie wyrażeń regularnych w C

Wyrażenia regularne w C pozwalają nie tylko na wyszukiwanie tekstu, ale również na jego podstawianie (zamiana) oraz dzielenie na części. Można również wykorzystać specjalne symbole i wyrażenia regularne, takie jak `+`, `*`, `?` czy `^`, aby bardziej precyzyjnie definiować wzorzec do wyszukania.

Przykładowo, można użyć następującego wyrażenia regularnego do wyszukania wszystkich liczb całkowitych z tekstu:

```
[0-9]+
```

Ponadto, w przypadku potrzeby bardziej zaawansowanych operacji, można również skorzystać z funkcji `regexec` z argumentem `REG_EXTENDED`, aby używać wyrażeń regularnych ze składnią zgodną z POSIX.

Warto również wspomnieć, że istnieją różnice w składni i możliwościach wyrażeń regularnych w różnych językach programowania, więc dokładne zapoznanie się z dokumentacją jest zalecane.

## Zobacz także

- [Dokumentacja wyrażeń regularnych w języku C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Wprowadzenie do wyrażeń regularnych w języku C](https://www.keycdn.com/support/regex-tutorial#c-code-example)
- [Wyrażenia regularne w C++](https://www.geeksforgeeks.org/regular-expressions-in-c-c/)

Dzięki użyciu wyrażeń regularnych w C, programowanie może stać się znacznie wygodniejsze i efektywniejsze. Zachęcam do eksperymentowania z nimi i wykorzystywania w swoich projektach!