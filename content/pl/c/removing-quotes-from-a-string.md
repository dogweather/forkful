---
title:                "Usuwanie cudzysłowów z ciągu znaków"
date:                  2024-01-26T03:38:08.378841-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie cudzysłowów z ciągu znaków oznacza usunięcie wszelkich znaków cudzysłowu — czy to pojedynczych ('') czy podwójnych ("") — które są częścią zawartości ciągu. Programiści robią to w celu oczyszczenia danych wejściowych, przygotowania danych do dalszego przetwarzania, lub uniknięcia błędów składni podczas pracy z ścieżkami plików i poleceniami w językach, które używają cudzysłowów do zaznaczania ciągów znaków.

## Jak to zrobić:

Oto funkcja w C, która usunie te irytujące cudzysłowy z twoich ciągów znaków:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Oryginał: %s\n", str);
    remove_quotes(str);
    printf("Oczyszczony: %s\n", str);
    return 0;
}
```

Przykładowe wyjście:

```
Oryginał: He said, "Hello, 'world'!"
Oczyszczony: He said, Hello, world!
```

## Dogłębna analiza

Usuwanie cudzysłowów z ciągu znaków to zadanie, które istnieje od zarania programowania, gdzie higiena danych była i nadal jest kluczowa do unikania błędów (takich jak ataki SQL injection) lub upewnienia się, że ciąg znaków może być bezpiecznie przekazany do systemów, które mogłyby pomylić cudzysłów za znak kontrolny.

Historycznie, różne języki radzą sobie z tym zadaniem na różne sposoby - niektóre mają wbudowane funkcje (jak `strip` w Pythonie), podczas gdy inne, takie jak C, wymagają ręcznej implementacji ze względu na skupienie na zapewnieniu programistom kontroli na niższym poziomie.

Alternatywy obejmują użycie funkcji bibliotecznych, takich jak `strpbrk`, do znajdowania cudzysłowów lub stosowanie wyrażeń regularnych (z bibliotekami takimi jak PCRE) do bardziej złożonych wzorców, chociaż może to być przesada dla samego usuwania cudzysłowów.

Implementacja powyżej po prostu przeszukuje każdy znak w ciągu, kopiując tylko znaki bez cudzysłowu do lokalizacji wskaźnika zapisu. Jest to efektywne, ponieważ odbywa się to w miejscu, bez potrzeby dodatkowej pamięci na ciąg wynikowy.

## Zobacz także

- [Funkcje biblioteki standardowej C](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [Zrozumienie wskaźników w C](https://www.learn-c.org/en/Pointers)
