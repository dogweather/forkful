---
title:                "C: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu C często używamy wiersza poleceń do przekazywania parametrów do naszych programów. Jednym z najczęściej spotykanych przypadków jest przekazywanie argumentów wiersza poleceń. Zapoznanie się z tym tematem jest ważne, ponieważ pozwala nam lepiej wykorzystać potencjał języka C.

## Jak to zrobić

Istnieją dwa sposoby na przekazywanie argumentów wiersza poleceń w języku C: przez opcjonalne parametry funkcji `main` lub przez funkcję `getopt()`. Pierwsza metoda jest prostsza i polega na wykorzystaniu trzeciego argumentu w funkcji `main`, który jest w rzeczywistości tablicą zawierającą wszystkie przekazane argumenty. Możemy wtedy przetwarzać je za pomocą pętli `for`. Natomiast druga metoda jest bardziej elastyczna i pozwala na przełączanie opcji przy użyciu krótkich nazw lub długich nazw z odpowiednimi flagami.

Poniższy przykład wykorzystuje metodę z opcjonalnymi parametrami funkcji `main`:

```C
#include <stdio.h>

int main(int argc, char *argv[])
{
    int i;

    for (i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Załóżmy, że nazwa naszego programu to `hello`. Uruchomienie go z wiersza poleceń przy użyciu komendy `./hello argument1 argument2` spowoduje wyświetlenie następującego wyniku:

```
Argument 0: ./hello
Argument 1: argument1
Argument 2: argument2
```

## Głębsza analiza

Ciekawym zjawiskiem jest to, że pierwszy argument zawsze jest nazwą programu, a nie przekazanym przez nas argumentem. Dzięki temu możemy wykorzystać kolejne argumenty, np. jako wartości dla zmiennych lub jako kontrolę warunków.

Inną przydatną funkcją jest `atoi()`, która konwertuje ciąg znaków na liczbę całkowitą. Dzięki temu możemy przekazywać liczby jako argumenty i przetwarzać je wewnątrz naszego programu.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o przekazywaniu argumentów wiersza poleceń w języku C, zapoznaj się z poniższymi linkami:
- [Dokumentacja funkcji `getopt()`](https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html)
- [Przykładowy program C wykorzystujący opcjonalne parametry funkcji `main`](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)