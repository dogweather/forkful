---
title:    "C: Wycinanie podciągów"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Wydobycie podciągów lub ustalenie fragmentów tekstu z danego ciągu znaków może być niezwykle przydatne, zwłaszcza podczas przetwarzania dużej ilości danych. W programowaniu, często potrzebujemy dostępu do konkretnych fragmentów tekstu lub musimy wyodrębnić pewne informacje z dłuższego ciągu znaków. W tym blogowym wpisie dowiesz się, jak wykorzystać funkcje języka C do wydobycia podciągów.

## Jak to zrobić

Aby wyodrębnić podciąg z danego ciągu znaków w języku C, musimy użyć funkcji `strncpy()`. Przyjmuje ona trzy parametry - wskaźnik do miejsca, gdzie chcemy zapisać podciąg, wskaźnik do miejsca, z którego chcemy wyodrębnić podciąg oraz liczbę znaków, które chcemy skopiować.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[] = "Blogowy wpis o wydobyciu podciągów";
    char str2[20];

    // skopiowanie podciągu od 14 znaku do końca
    strncpy(str2, &str1[14], 20);

    printf("Wyodrębiony podciąg: %s", str2);

    return 0;
}
```

Po uruchomieniu powyższego kodu, otrzymamy następujący wynik:

```
Wyodrębiony podciąg: o wydobyciu podciągów
```

## Deep Dive

W powyższym przykładzie wykorzystaliśmy funkcję `strncpy()`, jednak istnieje również wiele innych funkcji, które umożliwiają nam wydobycie podciągów lub fragmentów tekstu w języku C. Przykładowo, funkcja `strtok()` pozwala na rozbicie ciągu znaków na mniejsze podciągi na podstawie określonego separatora.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "24|67|90";

    // podzielenie ciągu na trzy podciągi
    char *token = strtok(str, "|");

    while (token != NULL) {
        printf("%s\n", token);
        token = strtok(NULL, "|");
    }

    return 0;
}
```

Powyższy kod wyświetli:

```
24
67
90
```

## Zobacz także

- [Dokumentacja języka C na temat funkcji do manipulacji ciągami znaków](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Przykładowe zadania na wydobycie podciągów w języku C](https://www.codewars.com/kata/55efecb8680f4769e5000029)