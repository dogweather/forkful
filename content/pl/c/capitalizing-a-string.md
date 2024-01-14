---
title:    "C: Zmiana pierwszej litery w ciągu znaków"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu zawsze istnieje potrzeba manipulacji łańcuchami znaków. Jedną z powszechnie stosowanych operacji jest kapitalizacja, czyli zamiana wszystkich liter w łańcuchu na duże. W tym artykule dowiesz się, dlaczego taka operacja jest często wykorzystywana.

## Jak to zrobić

Istnieje kilka sposobów na zastosowanie kapitalizacji w C. Pierwszym sposobem jest użycie funkcji `toupper()` z biblioteki `<ctype.h>`. Poniżej przedstawiono przykładowy kod, który zamienia wszystkie litery w łańcuchu na duże:

```C
#include <stdio.h>
#include <ctype.h>
#include <string.h>

int main()
{
    char str[100];

    printf("Podaj łańcuch znaków: ");
    fgets(str, 100, stdin);

    for (int i = 0; i < strlen(str); i++)
    {
        str[i] = toupper(str[i]);
    }

    printf("Zamieniony łańcuch: %s", str);

    return 0;
}
```

**Output:**

```
Podaj łańcuch znaków: hello world
Zamieniony łańcuch: HELLO WORLD
```

Kolejną popularną metodą jest użycie funkcji `strlwr()` z biblioteki `<string.h>`. Poniżej znajduje się kod, który zamienia wszystkie litery w łańcuchu na małe:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[100];

    printf("Podaj łańcuch znaków: ");
    fgets(str, 100, stdin);

    strlwr(str);

    printf("Zamieniony łańcuch: %s", str);

    return 0;
}
```

**Output:**

```
Podaj łańcuch znaków: HELLO WORLD
Zamieniony łańcuch: hello world
```

## Głębsza analiza

Kapitalizacja łańcucha może być wykorzystywana w różnych sytuacjach, na przykład do wyrównywania danych wejściowych lub do sprawdzania zgodności danych z określonym formatem. W przypadku prostej operacji konwersji na duże lub małe litery, do wyboru są dwie funkcje: `toupper()` i `tolower()`. Jeśli jednak mamy do czynienia ze słowami w innych językach, należy zwrócić uwagę na funkcje `toupper_l()` i `tolower_l()`, które są bardziej zaawansowane i obsługują różne zestawy znaków.

## Zobacz także

- [Funkcja toupper()](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [Funkcja strlwr()](https://www.tutorialspoint.com/c_standard_library/c_function_strlwr.htm)
- [Dokumentacja C - łańcuchy znaków](https://www.tutorialspoint.com/cprogramming/c_strings.htm)