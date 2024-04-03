---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:55.008600-07:00
description: "Tablice asocjacyjne, znane w innych j\u0119zykach jako mapy lub s\u0142\
  owniki, s\u0105 parami klucz-warto\u015B\u0107 u\u017Cywanymi do efektywnego wyszukiwania\
  \ i manipulowania danymi.\u2026"
lastmod: '2024-03-13T22:44:35.878565-06:00'
model: gpt-4-0125-preview
summary: "Tablice asocjacyjne, znane w innych j\u0119zykach jako mapy lub s\u0142\
  owniki, s\u0105 parami klucz-warto\u015B\u0107 u\u017Cywanymi do efektywnego wyszukiwania\
  \ i manipulowania danymi."
title: "U\u017Cywanie tablic asocjacyjnych"
weight: 15
---

## Jak to zrobić:
C nie posiada wbudowanego wsparcia dla tablic asocjacyjnych, jak niektóre języki wyższego poziomu, ale można je symulować za pomocą struktur i haszowania. Poniżej znajduje się uproszczony przykład użycia kombinacji struktury i prostej funkcji haszującej do implementacji tablicy asocjacyjnej do przechowywania i dostępu do liczb całkowitych za pomocą kluczy typu string.

Najpierw zdefiniuj strukturę do reprezentowania pojedynczej pary klucz-wartość oraz inną do reprezentowania samej tablicy asocjacyjnej:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} ParaKluczWartosc;

typedef struct {
    ParaKluczWartosc* items[TABLE_SIZE];
} TablicaAsoc;

unsigned int hash(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void initArray(TablicaAsoc* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(TablicaAsoc* array, char* key, int value) {
    unsigned int slot = hash(key);

    ParaKluczWartosc* item = (ParaKluczWartosc*)malloc(sizeof(ParaKluczWartosc));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(TablicaAsoc* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    TablicaAsoc a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // Wyjście: 1
    printf("%d\n", find(&a, "key2")); // Wyjście: 2

    return 0;
}
```

Przykład demonstruje podstawowe operacje: inicjalizację tablicy asocjacyjnej, wstawianie par klucz-wartość i wyszukiwanie wartości po kluczach. Zauważ, że ten kod nie obsługuje kolizji i jest przeznaczony do celów edukacyjnych.

## Pogłębienie
Koncepcja tablic asocjacyjnych jest starsza niż język C, ale niskopoziomowy charakter tego języka nie wspiera ich bezpośrednio jako wbudowanych typów. Zachęca to do głębszego zrozumienia struktur danych i algorytmów, w tym mechanizmów haszowania dla efektywnej mapowania klucz-wartość. Wiele bibliotek i frameworków C oferuje bardziej wyrafinowane podejścia do implementacji tablic asocjacyjnych, takie jak `GHashTable` z GLib, która zapewnia solidną implementację kompletną z obsługą kolizji, dynamicznym skalowaniem i wsparciem dla dowolnych typów kluczy i wartości.

Chociaż ręczna konstrukcja tablic asocjacyjnych w C może wydawać się uciążliwa w porównaniu z językami mającymi wbudowane wsparcie, oferuje ona bezcenne wglądy w działanie struktur danych, zaostrzając umiejętności programisty w rozwiązywaniu problemów i optymalizacji. Jednakże, dla kodu produkcyjnego lub bardziej złożonych aplikacji, korzystanie z istniejących bibliotek jak GLib często jest bardziej praktycznym i efektywnym czasowo podejściem.
