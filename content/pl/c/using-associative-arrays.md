---
title:                "Korzystanie z tablic asocjacyjnych"
date:                  2024-01-30T19:10:33.958024-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z tablic asocjacyjnych"
programming_language: "C"
category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Tablice asocjacyjne, czyli mapy hash, to pary klucz-wartość, które pozwalają na przechowywanie i odzyskiwanie danych za pomocą klucza. Są niezwykle użyteczne w języku C, ponieważ umożliwiają szybszy dostęp do danych w porównaniu do list, szczególnie gdy mamy do czynienia z dużą ilością danych.

## Jak to zrobić:

C nie ma wbudowanego wsparcia dla tablic asocjacyjnych tak jak niektóre inne języki, ale możemy użyć struktur i niektórych funkcji bibliotecznych, aby uzyskać podobną funkcjonalność. Oto prosta implementacja z użyciem biblioteki `uthash`, którą musisz dołączyć do swojego projektu.

Najpierw zdefiniuj strukturę do przechowywania par klucz-wartość:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // To będzie nasz klucz
    char name[10]; // To jest wartość skojarzona z naszym kluczem
    UT_hash_handle hh; // Sprawia, że ta struktura jest haszowalna
} osoba;
```

Następnie dodajmy kilka wpisów i odzyskajmy je:

```C
int main() {
    osoba *moje_osoby = NULL, *s;

    // Dodawanie wpisu
    s = (osoba*)malloc(sizeof(osoba));
    s->id = 1;
    strcpy(s->name, "Alicja");
    HASH_ADD_INT(moje_osoby, id, s);

    // Odzyskiwanie wpisu
    int user_id = 1;
    HASH_FIND_INT(moje_osoby, &user_id, s);
    if (s) {
        printf("Znaleziono: %s\n", s->name);
    }
    
    return 0;
}
```

Przykładowe wyjście:

```
Znaleziono: Alicja
```

Nie zapomnij zwolnić przydzielonej pamięci i dealokować tabelę hash po zakończeniu, aby uniknąć wycieków pamięci.

## Pogłębiona analiza

Chociaż tablice asocjacyjne nie są rodzime dla języka C, biblioteki takie jak `uthash` dobrze wypełniają tę lukę, oferując dość prosty sposób na korzystanie z tej funkcjonalności. Historycznie, programiści C musieli implementować własne wersje tych struktur danych, co prowadziło do różnych i często skomplikowanych implementacji, szczególnie dla tych, którzy dopiero zaczynają z językiem.

Pamiętaj, że efektywność używania tablic asocjacyjnych w C w dużej mierze zależy od tego, jak dobrze funkcja hash rozkłada wartości w tabeli, aby zminimalizować kolizje. Chociaż biblioteki takie jak `uthash` oferują dobrą równowagę między łatwością użycia a wydajnością, w krytycznych aplikacjach, gdzie wydajność jest najważniejsza, możesz chcieć dostosować lub zaimplementować własną tabelę hash.

Dla aplikacji wymagających maksymalnej wydajności, alternatywne struktury danych, a nawet inne języki programowania z wbudowanym wsparciem dla tablic asocjacyjnych, mogą być lepszym wyborem. Jednak w wielu sytuacjach, szczególnie gdy już pracujesz w środowisku C, korzystanie z biblioteki takiej jak `uthash` zapewnia praktyczną równowagę między wydajnością a wygodą.
