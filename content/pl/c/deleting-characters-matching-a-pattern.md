---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Czasami w programowaniu, trzeba usunąć znaki pasujące do określonego wzorca z ciągu znaków. Programiści robią to, aby optymalizować dane wejściowe, np. usuwając niepotrzebne spacje, tabulacje lub znaki nowego wiersza.

## Jak to Zrobić:

Poniżej jest kod w C, pokazujący jak usunąć określone znaki z ciągu:

```C
#include <stdio.h>
#include <string.h>

void usun_znaki(char *zrodlo, char *do_usuniecia) {
    int strIndex = 0;
    int resIndex = 0;
    int i;
    while (zrodlo[strIndex]) {
        for (i = 0; i<strlen(do_usuniecia); i++) {
            if (zrodlo[strIndex] == do_usuniecia[i]) break;
        }
        if (i == strlen(do_usuniecia)) zrodlo[resIndex++] = zrodlo[strIndex];
        strIndex++;
    }
    zrodlo[resIndex] = '\0';
}

int main() {
    char str[] = "Cześć, świat!";
    char znaki[] = ",";
    usun_znaki(str, znaki);
    printf("%s\n", str);
    return 0;
}
```

Powyższy kod usunie przecinki z ciągu "Cześć, świat!" i wyświetli "Cześć świat!".

## Deeper Dive

Powyższy kod jest skutecznym sposobem na usunięcie określonych znaków, ale jest kilka rzeczy, które są ważne do zapamiętania.

Po pierwsze, trzeba pamiętać, że kod działa na oryginalnym ciągu znaków. To znaczy, że oryginalny ciąg zostanie zmieniony, co może być problematyczne, gdy jest to niepożądane. 

Alternatywna strategia to utworzenie nowego ciągu, który nie zawiera niechcianych znaków. Wybór pomiędzy tymi dwiema strategiami zależy od specyficznych wymagań programu.

Dodatkowo, warto zauważyć, że ten kod używa funkcji `strlen()` w pętli, co sprawia, że jest mniej wydajny. W bardziej złożonych programach, optymalizacje takie jak przechowywanie długości ciągu znaków w zmiennej byłyby korzystne.

## Zobacz Też

- Dokumentacja GNU C Library: [String and Array Utilities](https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html)
- StackOverflow: [How to delete certain character from a string using C](https://stackoverflow.com/questions/5457608/how-to-remove-the-character-at-a-given-index-from-a-string-in-c)
- Kursy Programowania C: [Operacje na ciągach znaków](https://www.learn-c.org/pl/Strings)