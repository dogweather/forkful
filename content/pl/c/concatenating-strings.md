---
title:                "Łączenie ciągów znaków"
html_title:           "C: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konkatenacja lub łączenie ciągów znaków to proces łączenia dwóch lub więcej ciągów znaków w jeden dłuższy ciąg. Programiści często stosują tę operację, aby tworzyć wyrażenia lub komunikaty składające się z kilku części.

## Jak to zrobić:
```c
// Przykładowy kod w języku C
#include <stdio.h>

int main() {
    char message_one[] = "Witaj";
    char message_two[] = "Karol!";
    
    char concatenated[50]; // odpowiednio duża tablica dla naszych dwóch wiadomości
    
    // konkatenacja za pomocą funkcji sprintf
    sprintf(concatenated, "%s %s", message_one, message_two);

    printf("%s", concatenated);

    return 0;
}
```
Wynik:
```
Witaj Karol!
```

## W zagłębienie:
Konkatenacja ciągów znaków ma długą historię w programowaniu. W języku C jest to jedna z najczęściej stosowanych operacji, ale na przestrzeni lat powstały też inne metody, takie jak funkcja `strcat` czy operator `+` w języku Python. Warto pamiętać, że operacja ta może być czasochłonna i wymagać odpowiedniej alokacji pamięci dla nowego ciągu.

## Zobacz także:
- [Funkcja `strcat` w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Konkatenacja ciągów w języku Python](https://python.pl/konkatenacja-ciągów-znakowych/)
- [Porównanie wydajności konkatenacji w różnych językach programowania](https://benchmarksgame-team.pages.debian.net/benchmarksgame/fastest/csharp.html)