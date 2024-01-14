---
title:    "C: Łączenie łańcuchów znaków"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego konkatenacja jest ważna w języku C?

Konkatenacja jest procesem łączenia dwóch lub więcej ciągów znaków w jeden ciąg. W języku C jest to często wykorzystywana operacja, ponieważ pozwala na manipulację tekstem w sposób wygodny i efektywny. W tym artykule dowiesz się, dlaczego konkatenacja jest ważna i jak jej używać w swoich programach.

## Jak używać konkatenacji w języku C?

Aby skonkatenować dwa ciągi znaków w języku C, możesz użyć funkcji `strcat()`. Przyjmuje ona dwa parametry - pierwszy to ciąg, do którego chcesz dodać kolejny ciąg, a drugi to ciąg, który chcesz dodać. Poniżej znajduje się przykładowy kod pokazujący to w działaniu:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Hello ";
    char str2[20] = "world";
    strcat(str1, str2);
    printf("%s", str1);
    return 0;
}
```
Output: `Hello world`

W tym przykładzie, funkcja `strcat()` dodaje ciąg `str2` na koniec ciągu `str1`, zmieniając wartość `str1` na `Hello world`.

Możesz również konkatenować więcej niż dwa ciągi za pomocą wielokrotnego użycia funkcji `strcat()`, na przykład:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Cześć ";
    char str2[20] = "jak się masz, ";
    char str3[20] = "moje imię to ";
    char str4[20] = "Jan.";
    strcat(str1, str2);
    strcat(str1, str3);
    strcat(str1, str4);
    printf("%s", str1);
    return 0;
}
```
Output: `Cześć jak się masz, moje imię to Jan.`

## Wgląd w konkatenację ciągów w języku C

W języku C konkatenacja ciągów może być wykonywana również za pomocą operatora `+` lub za pomocą funkcji `sprintf()`. Operator `+` może być używany tylko w przypadku, gdy łączysz dwa ciągi stałe, takie jak:

```C
printf("Wartość PI wynosi: " + "3.14");
```
Output: `Wartość PI wynosi: 3.14`

Natomiast funkcja `sprintf()` może być używana do konkatenacji zmiennych oraz stałych w liczbie większej niż dwa ciągi, na przykład:

```C
#include <stdio.h>

int main() {
    char str1[20] = "Wartość PI wynosi: ";
    double pi = 3.14159;
    sprintf(str1, "%s%.2f", str1, pi);
    printf("%s", str1);
    return 0;
}
```
Output: `Wartość PI wynosi: 3.14`

## Zobacz również

- [Strona z dokumentacją funkcji strcat()](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Inne przydatne funkcje do manipulacji tekstem w języku C](https://www.programiz.com/c-programming/string-handling-functions)