---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:44.661135-07:00
description: "Jak to zrobi\u0107: W C, standardowo u\u017Cywan\u0105 funkcj\u0105\
  \ do znajdowania d\u0142ugo\u015Bci stringa jest `strlen()`. Oto kr\xF3tki przyk\u0142\
  ad."
lastmod: '2024-03-13T22:44:35.876273-06:00'
model: gpt-4-0125-preview
summary: "W C, standardowo u\u017Cywan\u0105 funkcj\u0105 do znajdowania d\u0142ugo\u015B\
  ci stringa jest `strlen()`."
title: "Znajdowanie d\u0142ugo\u015Bci \u0142a\u0144cucha"
weight: 7
---

## Jak to zrobić:
W C, standardowo używaną funkcją do znajdowania długości stringa jest `strlen()`. Oto krótki przykład:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Długość '%s' wynosi %zu.\n", myString, length);
    
    return 0;
}
```

**Przykładowy wynik:**
```
Długość 'Hello, World!' wynosi 13.
```

W tym przykładzie, `strlen()` przyjmuje string (`myString`) jako wejście i zwraca jego długość bez terminatora nulowego. Zaleca się użycie `size_t` dla zmiennej długości, ponieważ jest to typ całkowity bez znaku, zdolny do reprezentowania rozmiaru największego możliwego obiektu w systemie.

## Szczegółowa analiza:
Funkcja `strlen()` jest częścią standardowej biblioteki C od początku istnienia języka. W tle działa, inkrementując licznik, gdy przemierza string aż do napotkania terminatora nulowego. Ta prostota ma jednak swoje konsekwencje wydajnościowe: ponieważ `strlen()` liczy znaki w czasie rzeczywistym, wielokrotne jej wywoływanie na tym samym łańcuchu znaków, na przykład w pętli, jest nieefektywne.

Pod względem bezpieczeństwa, `strlen()` i inne funkcje obsługi stringów w C, nie sprawdzają automatycznie przepełnienia bufora, co czyni staranne programowanie kluczowym do uniknięcia podatności. Nowoczesne alternatywy w innych językach, takie jak typy stringów, które zawierają długość lub domyślnie obsługują bezpieczne buforowanie, eliminują niektóre z tych ryzyk i nieefektywności.

Pomimo swoich ograniczeń, zrozumienie `strlen()` i ręcznej obsługi stringów w C jest kluczowe dla programistów, szczególnie przy pracy z kodem niskopoziomowym lub gdy kluczowe są wydajność i kontrola pamięci. Pozwala to również na cenne wglądy w działanie abstrakcji stringów wyższego poziomu w innych językach.
