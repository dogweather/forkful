---
title:                "Obsługa błędów"
date:                  2024-01-26T00:49:55.625916-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obsługa błędów"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/handling-errors.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obsługa błędów w języku C polega na oczekiwaniu na niespodziewane. Chroni to programy przed wpadnięciem w chaos, gdy napotkają problemy. Programiści robią to, aby elegancko radzić sobie z błędami i utrzymać niezawodność swojego kodu.

## Jak to zrobić:

Zobaczmy, jak to zrobić w C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nieistniejacyplik.txt", "r");
    if (fp == NULL) {
        perror("Błąd otwarcia pliku");
        return EXIT_FAILURE;
    }
    // Coś zrób z plikiem
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Przykładowy wynik, gdy plik nie istnieje:
```
Błąd otwarcia pliku: Nie ma takiego pliku ani katalogu
```

## Dogłębna analiza

W początkowych dniach języka C, obsługa błędów była bardzo podstawowa – głównie kody zwrotne i ręczne sprawdzanie. Wprowadzono `errno`, globalną zmienną aktualizowaną, gdy funkcje zawiodą. Sama w sobie nie jest bezpieczna przy użyciu wątków, dlatego wprowadzono nowsze funkcje `strerror` i `perror` dla lepszego raportowania błędów.

Alternatywy? Nowoczesny C nie jest ograniczony do `errno`. Istnieją setjmp i longjmp do skoków nielokalnych, gdy nastąpi katastrofa. Niektórzy preferują definiowanie własnych kodów błędów, podczas gdy inni optują za strukturami podobnymi do wyjątków w C++.

Szczegóły implementacji mogą być skomplikowane. Na przykład `errno` jest bezpieczna w systemach zgodnych z POSIX dzięki magii lokalnego magazynu dla wątków (TLS). W systemach wbudowanych, gdzie zasoby są cenne, własna obsługa błędów może być preferowana nad standardowymi podejściami, które mogą zawyżyć rozmiar oprogramowania.

## Zobacz także

- Szczegółowa analiza `errno`: https://en.cppreference.com/w/c/error/errno
- Dla bezpieczeństwa wątków, zobacz wątki POSIX i errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- Wprowadzenie do setjmp i longjmp: https://www.cplusplus.com/reference/csetjmp/
- Dla obsługi wyjątków w C++, sprawdź: https://isocpp.org/wiki/faq/exceptions
