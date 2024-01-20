---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wyodrębnianie podciągów (ang. substring extraction) to proces pozyskiwania pewnej części ciągu znaków. Programiści robią to, aby manipulować danymi, analizować teksty lub filtrować wartości.

## Jak to zrobić:

Zobaczmy kod demonstrujący to zadanie. Wykorzystamy funkcję `strncpy`, która należy do biblioteki standardowej języka C:

```C
#include <stdio.h>
#include <string.h>

int main () {
   char src[20] = "Programowanie w C";
   char dest[15];
   
   strncpy(dest, src+5, 13);
   dest[13] = '\0'; // End of string character

   printf("Podciąg to: %s\n", dest);

   return 0;
}
```

Wynik to:

```C
Podciąg to: amowanie w C
```

## Pogłębione badanie

Ekstrakcja podciągów ma swoje korzenie w manipulacji tekstem i przetwarzaniu danych. Ta technika jest powszechnie wykorzystywana w wielu dziedzinach, takich jak przeglądanie stron internetowych, analiza danych i uczenie maszynowe. 

Istnieje wiele alternatyw dla `strncpy`, takich jak `memcpy`, `memmove` czy napisane przez użytkownika funkcje. Każda z nich ma swoje własne zastosowania w zależności od specyfiki problemu.

Większość funkcji ekstrakcji podciągów działa na tych samych zasadach. Pobiera wskaźnik do źródłowego ciągu znaków i kopiuje zdefiniowaną liczbę znaków do ciągu docelowego.

## Zobacz także:

Sprawdź te linki, aby dowiedzieć się więcej:

- Opracowanie na temat manipulacji tekstem w języku C: https://cboard.cprogramming.com/c-programming/154649-extract-substring-string-using-c.html
- Stack Overflow dla dyskusji o związanym kodzie: https://stackoverflow.com/questions/4214314/get-a-substring-of-a-char
- Dokumentacja języka C na temat funkcji `strncpy`: https://www.cplusplus.com/reference/cstring/strncpy/