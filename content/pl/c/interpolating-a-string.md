---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:50:12.577280-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
String interpolation to sposób bezpośredniego wstawiania zmiennych do stringów. Robimy to dla wygody i czytelności kodu – zamiast sklejać stringi, zastępujemy symbole specjalne wartościami.

## How to: (Jak to zrobić:)
W C interpolacja stringów nie jest wbudowana jak w niektórych językach wyższego poziomu, więc używamy `printf` lub `sprintf`. Oto przykład:

```c
#include <stdio.h>

int main() {
    int age = 25;
    char *name = "Jan";

    printf("Cześć, nazywam się %s i mam %d lat.\n", name, age);

    return 0;
}
```

Wyjście:
```
Cześć, nazywam się Jan i mam 25 lat.
```

## Deep Dive (Dogłębna analiza)
Interpolacja stringów nie jest bezpośrednio obsługiwana w C, jak w Pythonie czy JavaScript. Historia C pokazuje, że skupiano się na prostocie i wydajności, a nie na 'cukierkach' syntaktycznych. Alternatywy to konkatenacja za pomocą `strcat` lub bibliotek takich jak `asprintf` w GNU C. O implementacji – `printf` korzysta z formatowania, gdzie `%s`, `%d` itp. to symbole zastępcze na typ danej zmiennej, co C robi podczas kompilacji.

## See Also (Zobacz również)
- [printf format specifiers](http://www.cplusplus.com/reference/cstdio/printf/)
