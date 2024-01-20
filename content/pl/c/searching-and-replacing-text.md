---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?
Szukanie i zamiana tekstu to podstawowe operacje, dzięki którym możemy manipulować danymi w programach. Programiści robią to, aby przekształcać, filtrować czy korygować tekst na potrzeby swoich aplikacji.

## Jak to zrobić:

Spróbujmy szukać i zastąpić tekst za pomocą prostego przykładu.
```C
#include <string.h>

int main() {
    char text[] = "Witam Cieszę się, że omawiamy programowanie w C.";
    char *szukaj = "C";
    char *zamiana = "C++";
    char *wynik;

    wynik = strstr(text, szukaj);

    if(wynik) {
    	strncpy(wynik, zamiana, strlen(zamiana));
    	puts(text);
    }

    return 0;
}
```
Jeżeli uruchomisz ten kod, otrzymasz tekst: "Witam C++ieszę się, że omawiamy programowanie w C.".

## Pogłębiamy temat

Szukanie i zamiana tekstu to jeden z najstarszych trików w programowaniu, obecny w niemal każdym języku programowania. Istnieją również inne metody, takie jak wyrażenia regularne, które mogą być bardziej elastyczne, ale są trudniejsze do opanowania. Szczegóły implementacji naszego podejścia do szukania i zamiany opierają się na bibliotece string.h z języka C, która zawiera funkcje takie jak strstr() do wyszukiwania ciągów znaków i strncpy() do ich zamiany.

## Zobacz również

Poznaj więcej o manipulacji tekstem w C:
1. String.h biblioteki C: https://www.tutorialspoint.com/c_standard_library/string_h.htm
2. Wyrażenia regularne w C: https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
3. Inne techniki manipulacji tekstem w C: https://www.geeksforgeeks.org/string-handling-in-c-set-1/