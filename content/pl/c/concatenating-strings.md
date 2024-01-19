---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Łączenie łańcuchów znakowych, znane jako "concatenation", pozwala nam łączyć dwa lub więcej łańcuchów znakowych w jeden. Programiści robią to, aby tworzyć dynamiczne dane wyjściowe, łącząc wartości zmiennych z ciągłymi ciągami tekstowymi.

## Jak to zrobić:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char pierwszyLancuch[] = "Cześć, ";
    char drugiLancuch[] = "to jest łańcuch znaków.";
    char polaczonyLancuch[50];

    strcpy(polaczonyLancuch, pierwszyLancuch);
    strcat(polaczonyLancuch, drugiLancuch);

    printf("%s\n", polaczonyLancuch);  // Wyjście: Cześć, to jest łańcuch znaków.
    return 0;
}
```

## Głębsze zrozumienie:

Łączenie łańcuchów znakowych jest istotnym konceptem, który powstał już we wczesnych językach programowania. W C, proces ten opiera się na przeniesieniu null-terminatora łańcucha źródłowego do końca łańcucha docelowego, a następnie kopii pozostałych znaków.
Alternatywą jest użycie funkcji `sprintf()`, która może być nieco bardziej elastyczna, ale również nieco mniej efektywna pod względem wydajności. Co więcej, łączenie łańcuchów znakowych jest operacją kosztowną. Dlatego, zawsze warto używać tego rozwiązania z rozwagą.

## Zobacz także:

- [Dokumentacja strcat()](http://www.cplusplus.com/reference/cstring/strcat/)
- [Dokumentacja strcpy()](http://www.cplusplus.com/reference/cstring/strcpy/)
- [Szczegóły na temat łączenia łańcuchów znakowych w C](https://www.geeksforgeeks.org/concatenating-strings-in-c/)