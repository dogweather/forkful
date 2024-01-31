---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:46:49.921844-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co & Dlaczego?)
W C obliczanie długości łańcucha znaków to po prostu zliczanie znaków aż do osiągnięcia terminatora null (`\0`). To kluczowe, bo dzięki temu wiemy, ile danych przetwarzamy, zarządzamy pamięcią, czy też unikamy przekroczeń bufora.

## How to: (Jak to zrobić:)
```C
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Witaj, świecie!";

    // Obliczanie długości stringa funkcją strlen.
    int dlugosc = strlen(text);

    printf("Długość łańcucha znakowego: %d\n", dlugosc);
    return 0;
}
```
Output:
```
Długość łańcucha znakowego: 15
```

## Deep Dive (Dogłębna Analiza)
Kiedyś, przed funkcją `strlen` z biblioteki `string.h`, programiści musieli samodzielnie iterować przez łańcuch zliczając znaki. Alternatywą jest pisania własnej funkcji, co może być bardziej wydajne w specjalnych przypadkach (np. liczenie długości tylko do pewnego znaku). 

Implementując własną funkcję, pamiętaj o terminatorze null, który oznacza koniec stringa. Oto przykład minimalistycznej implementacji:

```C
int custom_strlen(const char *str) {
    int count = 0;
    while(*str++)
        count++;
    return count;
}
```

Różnice w wydajności między `strlen` a własną funkcją bywają znikome, ale `strlen` jest zazwyczaj zoptymalizowana przez kompilator.

## See Also (Zobacz Również)
- Dokumentacja `strlen` na stronie [cppreference.com](https://en.cppreference.com/w/c/string/byte/strlen)
- Przykłady przekroczeń bufora (buffer overflows) na [owasp.org](https://owasp.org/www-community/vulnerabilities/Buffer_Overflow)
- Porównanie funkcji z `string.h` z własnymi implementacjami na [stackoverflow.com](https://stackoverflow.com/questions/tagged/c+string)
