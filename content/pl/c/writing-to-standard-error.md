---
title:                "Pisanie do standardowego błędu"
html_title:           "C: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?:

W programowaniu, "wypisywanie do standardowego błędu" lub po prostu "wypisywanie na stderr" odnosi się do wysyłania komunikatów błędów lub innych informacji o błędach do standardowego strumienia błędów. Programiści wykorzystują to do informowania użytkowników o wystąpieniu błędów w programie i pomagają w ich zlokalizowaniu.

## Jak to zrobić:

```c
#include <stdio.h>

int main() {

    // Przykładowy kod, który wywołuje błąd
    int number = 0;
    int divisor = 0;

    int result = number / divisor;

    return 0;
}
```

W powyższym przykładzie, wywołanie błędu spowoduje wypisanie informacji o błędzie na stderr:

```
Błąd dzielenia przez zero
```

## Głębokie zanurzenie:

Istnieją różne sposoby wypisywania informacji o błędach w C, ale wypisywanie na stderr jest popularnym i ustandaryzowanym sposobem. Alternatywnie, programiści mogą wypisywać na standardowe wyjście (`printf()`), ale będzie to powodować mieszanie wiadomości o błędach z innymi danymi programu. Implementacja polega na wysłaniu ciągu znaków zawierającego informacje o błędzie do standardowego strumienia błędów.

## Zobacz także:

- [Dokumentacja funkcji `fprintf()`](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Porównanie standardowych strumieni w C](https://www.geeksforgeeks.org/comparison-standard-input-output-streams-c/)
- [Artykuł o wypisywaniu na stderr w C++](https://www.techiedelight.com/redirect-both-stdout-stderr-file-cpp/)