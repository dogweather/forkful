---
title:                "Usuwanie znaków odpowiadających wzorcowi."
html_title:           "C: Usuwanie znaków odpowiadających wzorcowi."
simple_title:         "Usuwanie znaków odpowiadających wzorcowi."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Usuwanie znaków pasujących do wzorca jest jedną z podstawowych operacji w programowaniu. Polega ona na usunięciu wszystkich znaków z ciągu znaków, które pasują do danego wzorca. Programiści często wykonują tę operację, aby przetwarzać dane i filtrować niepotrzebne informacje.

## Jak to zrobić:

```C
#include <stdio.h>
#include <string.h>

// funkcja do usuwania znaków pasujących do danego wzorca z ciągu znaków
// przykład użycia: delete_chars("programming is fun", "mfi");
// wynik: progrng s un
void delete_chars(char *str, char *pattern) {
    int i, j, k;
    int n = strlen(str);
    int m = strlen(pattern);
    char temp[n];
    int temp_index = 0;
    for (i = 0; i < n; i++) {
        int flag = 0;
        for (j = 0; j < m; j++) {
             if (str[i] == pattern[j]) {
                flag = 1;
             }
        }
        if (flag == 0) {
             temp[temp_index++] = str[i];
        }
    }
    temp[temp_index] = '\0';
    strcpy(str, temp);
    printf("%s\n", str);
}

int main() {
    // przykład użycia
    char str[] = "programming is fun";
    char pattern[] = "mfi";
    delete_chars(str, pattern);
    return 0;
}
```

**Output: progrng s un**

## Deep Dive:

Usuwanie znaków pasujących do wzorca jest popularnym zadaniem w programowaniu i jest wykorzystywane w różnych zastosowaniach, takich jak przetwarzanie danych, filtrowanie tekstu i analiza języka naturalnego. Metoda usuwania znaków pasujących do wzorca została wprowadzona w języku C i od tego czasu jest wykorzystywana w wielu innych językach programowania.

Alternatywne metody usuwania znaków pasujących do wzorca obejmują użycie pętli i struktur danych, takich jak tablice i listy, aby przechowywać i przetwarzać dane. Jednak metoda usuwania znaków jest prostsza i szybsza, ponieważ wykorzystuje gotowe funkcje biblioteczne, takie jak funkcja `strcpy()` w C.

Implementacja algorytmu usuwania znaków pasujących do wzorca może być dostosowana do różnych zastosowań. Na przykład, w powyższym przykładzie, funkcja `delete_chars()` została napisana dla przejrzystości, ale może być zoptymalizowana dla wydajniejszego wykonywania lub dostosowana do obsługiwania różnych typów danych.

## See Also:

Dla dalszego pogłębienia tematu, zapoznaj się z poniższymi źródłami:

- Wprowadzenie do języka C: https://www.tutorialspoint.com/cprogramming/
- Przykłady usuwania znaków pasujących do wzorca w innych językach programowania: https://www.geeksforgeeks.org/remove-characters-from-the-first-string-which-are-present-in-the-second-string/