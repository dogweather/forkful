---
title:                "Wyszukiwanie i zamiana tekstu"
date:                  2024-01-20T17:57:33.635017-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wyszukiwanie i zamiana tekstu"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Wyszukiwanie i zamiana tekstu to operacje, które umożliwiają szybką edycję ciągów znaków - od prostego zastępowania słów po zaawansowane przetwarzanie danych. Programiści używają tych operacji, by naprawiać błędy, aktualizować informacje i ogólnie ulepszać interakcję z tekstem w aplikacjach.

## How to: (Jak to zrobić?)
Przykład w C, który pokazuje, jak szukać i zastępować tekst:

```C
#include <stdio.h>
#include <string.h>

void searchReplace(char *str, const char *search, const char *replace) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = str;
    size_t search_len = strlen(search);
    size_t replace_len = strlen(replace);

    while (1) {
        const char *p = strstr(tmp, search);

        // jeśli nie znaleziono szukanego tekstu zakończ pętlę
        if (p == NULL) {
            strcpy(insert_point, tmp);
            break;
        }

        // kopiowanie części przed wystąpieniem szukanego tekstu
        memcpy(insert_point, tmp, p - tmp);
        insert_point += p - tmp;

        // wstawiamy nowy tekst
        memcpy(insert_point, replace, replace_len);
        insert_point += replace_len;

        // aktualizujemy tmp, przesuwając się za zastąpiony tekst
        tmp = p + search_len;
    }

    // kopiujemy z bufora do oryginalnego miejsca
    strcpy(str, buffer);
}

int main() {
    char text[] = "Ala ma kota, kot ma Ale.";

    searchReplace(text, "kot", "pies");
    printf("Zmieniony tekst: %s\n", text);

    return 0;
}
```
Output:
```
Zmieniony tekst: Ala ma piesa, pies ma Ale.
```

## Deep Dive (Dogłębna analiza)
Wczesne komputery operowały na prostych tekstach - wyszukiwanie i zamiana były jednymi z podstawowych operacji. Dziś, mimo rozwiniętych edytorów i IDE, te operacje wciąż są kluczowe dla automatyzacji i pracy z kodem źródłowym.

Alternatywy:
- Regex (wyrażenia regularne) - dla bardziej skomplikowanych wzorców tekstowych.
- Funkcje wbudowane - języki wyższego poziomu oferują bardziej złożone i elastyczne metody.

Szczegóły implementacji:
Szukanie tekstu odbywa się przez `strstr()`, a realokacja tekstu przez bufory tymczasowe, aby uniknąć nadpisania danych w trakcie operacji.

## See Also (Zobacz też)
- [GNU C Library: Searching and Sorting](https://www.gnu.org/software/libc/manual/html_node/Searching-and-Sorting.html)
- [Stack Overflow: Implementing str_replace](https://stackoverflow.com/questions/779875/what-is-the-function-to-replace-string-in-c)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
