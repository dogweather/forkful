---
title:                "C: Wyszukiwanie i zastępowanie tekstu"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Podczas pisania oprogramowania często zdarza się, że chcemy dokonać zmian w tekście. Może to być konieczne podczas tworzenia raportów, przetwarzania danych lub po prostu w celu ułatwienia sobie pracy. W takich sytuacjach użytecznym narzędziem jest funkcja "szukaj i zamień", która pozwala na szybkie i dokładne zmiany w tekście.

## Jak to zrobić

Aby wykonać "szukaj i zamień" w języku C, musisz użyć funkcji `str_replace()`. Najpierw musisz zadeklarować dwie zmienne typu `char` - jedną zawierającą tekst, w którym będziemy szukać, a drugą z tekstem, który będzie podmieniany. Następnie, używając pętli `for`, możesz przeiterować po tekście i sprawdzić, czy zawiera ona szukaną frazę, a następnie użyć funkcji `strcpy()` do zastąpienia jej nowym tekstem.

```
#include <stdio.h>
#include <string.h>

char text[] = "Blog post for Polish readers using Markdown";
char search[] = "Polish";
char replace[] = "Polski";

void str_replace(char *original, char *search, char *replace) {
    char *result;
    int i, cnt = 0;
    int new_len = strlen(replace);
    int old_len = strlen(search);
    
    for (i = 0; original[i] != '\0'; i++) {
        if (strstr(&original[i], search) == &original[i]) {
            cnt++;
            i += old_len - 1;
        }
    }

    result = (char *)malloc(i + cnt * (new_len - old_len) + 1);

    i = 0;
    while (*original) {
        if (strstr(original, search) == original) {
            strcpy(&result[i], replace);
            i += new_len;
            original += old_len;
        } else
            result[i++] = *original++;
    }

    result[i] = '\0';
    printf("%s\n", result);
}

int main() {
    printf("Original text: %s\n", text);
    printf("Modified with str_replace: ");
    str_replace(text, search, replace);
    return 0;
}
```

Wynikiem działania tego kodu będzie:

```
Original text: Blog post for Polish readers using Markdown
Modified with str_replace: Blog post for Polski readers using Markdown
```

## Głębszy zanurzanie

Chociaż funkcja `str_replace()` jest bardzo przydatna do szybkiej zmiany tekstu, warto zwrócić uwagę, że wymaga ona podania dokładnej frazy, którą chcemy zamienić. Jeśli chcesz być bardziej elastyczny w wyborze tekstu, który ma zostać zamieniony, możesz użyć innych funkcji, takich jak `strtok()` lub `regexp()`.

Funkcja `strtok()` pozwala na podzielenie tekstu na mniejsze części, które można edytować lub zamienić. Natomiast `regexp()` pozwala na dopasowanie tekstu do wyrażeń regularnych, co jest szczególnie przydatne w przypadku, gdy chcesz zmienić całą grupę wyrazów za jednym razem.

## Zobacz także

- Funkcja `str_replace()` w języku C: https://www.tutorialspoint.com/c_standard_library/c_function_str_replace.htm
- Wyrażenia regularne w języku C: https://www.tutorialspoint.com/c_standard_library/c_function_regexp.htm
- Podstawy Markdown: https://www.markdownguide.org/basic-syntax/