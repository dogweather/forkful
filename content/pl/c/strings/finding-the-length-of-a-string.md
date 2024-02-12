---
title:                "Znajdowanie długości łańcucha"
aliases:
- /pl/c/finding-the-length-of-a-string/
date:                  2024-02-03T17:56:44.661135-07:00
model:                 gpt-4-0125-preview
simple_title:         "Znajdowanie długości łańcucha"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/finding-the-length-of-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Znalezienie długości stringa w C polega na określeniu liczby znaków przed terminator nulowym `\0`. Programiści robią to, aby poprawnie manipulować ciągami znaków, nie napotykając na błędy takie jak przepełnienia bufora, które mogą prowadzić do luk w zabezpieczeniach lub awarii programu.

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
