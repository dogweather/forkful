---
title:    "C: Odczytywanie argumentów z wiersza poleceń"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak wiele informacji może być przekazane do programu z linii poleceń? Dzięki argumentom wiersza poleceń, jest to możliwe. W tym poście dowiesz się, dlaczego warto nauczyć się czytać argumenty wiersza poleceń w języku C.

## Jak To Zrobić

Aby odczytać argumenty wiersza poleceń w języku C, należy przejść przez kilka kroków:

1. W pierwszej kolejności należy zaimportować bibliotekę <stdio.h>.

```C
#include <stdio.h>
```

2. Następnie zadeklaruj zmienną **argc**, która będzie przechowywać liczbę argumentów wiersza poleceń, oraz dwuwymiarową tablicę **argv**, która przechowa wartości tych argumentów.

```C
int argc;
char *argv[MAX_ARGS];
```

3. W funkcji **main** należy przekazać parametry **argc** i **argv**, które są argumentami wiersza poleceń.

```C
int main(int argc, char *argv[])
```

4. Następnie można wypisać wartości argumentów wiersza poleceń za pomocą pętli **for**.

```C
for(int i = 0; i < argc; i++)
{
    printf("Argument %d: %s\n", i, argv[i]);
}
```

5. Przejdź do konsoli i uruchom program z argumentami, np. ```./program nazwa 123```.

6. Otrzymasz następujące wyjście:

```bash
Argument 0: ./program
Argument 1: nazwa
Argument 2: 123
```

## Deep Dive

W przypadku gdy program wymaga więcej niż jednego argumentu, należy pamiętać o indeksowaniu argumentów od **0** do **argc-1**.

Istnieje również możliwość przekazania argumentów opcjonalnych i opcji wiersza poleceń, co jest bardzo przydatne w większych programach.

Przed zastosowaniem argumentów wiersza poleceń, należy sprawdzić ich poprawność i dbać o bezpieczeństwo programu, aby uniknąć niechcianych luk w zabezpieczeniach.

## Zobacz również

- [Dokumentacja C - Argumenty wiersza poleceń](https://pl.wikibooks.org/wiki/C/Argumenty_funkcji)
- [Poradnik: Czytanie argumentów z linii poleceń w języku C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Kurs C - Argumenty wiersza poleceń](http://forc.org.pl/~lukasiewicz/jp/www/cz-args.html)