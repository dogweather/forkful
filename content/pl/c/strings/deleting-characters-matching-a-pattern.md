---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:32.802939-07:00
description: "Jak to zrobi\u0107: C nie posiada wbudowanej funkcji do bezpo\u015B\
  redniego usuwania znak\xF3w z ci\u0105gu na podstawie wzorca, w przeciwie\u0144\
  stwie do niekt\xF3rych j\u0119zyk\xF3w\u2026"
lastmod: '2024-03-13T22:44:35.868291-06:00'
model: gpt-4-0125-preview
summary: "C nie posiada wbudowanej funkcji do bezpo\u015Bredniego usuwania znak\xF3\
  w z ci\u0105gu na podstawie wzorca, w przeciwie\u0144stwie do niekt\xF3rych j\u0119\
  zyk\xF3w wy\u017Cszego poziomu."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## Jak to zrobić:
C nie posiada wbudowanej funkcji do bezpośredniego usuwania znaków z ciągu na podstawie wzorca, w przeciwieństwie do niektórych języków wyższego poziomu. Jednak łatwo możesz osiągnąć ten cel, ręcznie iterując przez ciąg i budując nowy, który wyklucza niechciane znaki. Na przykład, załóżmy, że chcesz usunąć wszystkie cyfry z ciągu. Możesz to zrobić następująco:

```c
#include <stdio.h>
#include <ctype.h>

void remove_digits(char *str) {
    char *src = str, *dst = str;
    while (*src) {
        if (!isdigit((unsigned char)*src)) {
            *dst++ = *src;
        }
        src++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "Programowanie w C 101: Podstawy!";
    remove_digits(str);
    printf("Wynik: %s\n", str);
    return 0;
}
```

Przykładowe wyjście:
```
Wynik: Programowanie w C : Podstawy!
```

Ten przykład korzysta z funkcji `isdigit` z biblioteki `ctype.h`, aby identyfikować cyfry, przesuwając znaki, które nie są cyframi, na początek ciągu i kończąc ciąg, gdy wszystkie znaki zostaną ocenione.

## Pogłębienie
Prezentowane rozwiązanie korzysta z podejścia z dwoma wskaźnikami w tej samej tablicy, aby skutecznie filtrować niechciane znaki, technika będąca znakiem rozpoznawczym filozofii zarządzania pamięcią w C. Ta metoda jest wydajna, ponieważ działa w miejscu, unikając potrzeby dodatkowej alokacji pamięci i minimalizując tym samym obciążenie.

Historycznie rzecz biorąc, brak zaawansowanych funkcji manipulacji ciągami w C zmuszał programistów do rozwijania głębokiego zrozumienia obsługi ciągów na poziomie pamięci, prowadząc do innowacyjnych podejść, takich jak powyższe. Chociaż ma to zaletę większej kontroli i wydajności, wiąże się z większym ryzykiem błędów, takich jak przepełnienie bufora i błędy o jeden za mało.

We współczesnych kontekstach rozwojowych, zwłaszcza tych, które kładą nacisk na bezpieczeństwo i ochronę, języki abstrahujące takie operacje niskiego poziomu mogą być preferowane do zadań manipulacji ciągami. Niemniej jednak zrozumienie i wykorzystanie tych technik C pozostaje nieocenione w scenariuszach wymagających optymalizacji wydajności na najdrobniejszym poziomie lub przy pracy w środowiskach, gdzie minimalizm i szybkość C są kluczowe.
