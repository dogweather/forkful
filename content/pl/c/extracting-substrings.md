---
title:                "Ekstrakcja podciągów"
html_title:           "C: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/extracting-substrings.md"
---

{{< edit_this_page >}}

##Czego i dlaczego?
Wyodrębnianie podciągów to częsta praktyka programistów polegająca na uzyskiwaniu fragmentów tekstu z większego ciągu znaków. Ma to wiele zastosowań, w tym weryfikację poprawności wprowadzanych danych, przetwarzanie danych w formacie tekstowym oraz tworzenie właściwych wyświetleń i reprezentacji tekstowych.

## Jak to zrobić:
Wyodrębnianie podciągów jest stosunkowo proste za pomocą wbudowanych funkcji biblioteki C. Przykładowy kod dla tekstu "Hello World" i wyodrębnienia podciągu "World" jest następujący:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Hello World";
  char substr[] = "World";
  char *ptr = strstr(str, substr);
  if (ptr) {
    printf("Podciąg znaleziony: %s\n", ptr);
  } else {
    printf("Podciąg nie został znaleziony.\n");
  }
  return 0;
}
```

Wynik tego programu będzie wyglądać następująco:

```
Podciąg znaleziony: World
```

Można także wykorzystać funkcje takie jak `strncpy` lub `strtok` do bardziej zaawansowanych operacji na podciągach.

## Zanurzenie w temat:
Wyodrębnianie podciągów było powszechnie stosowane w językach programowania jeszcze przed pojawieniem się standardu języka C. W najnowszej wersji języka, wyodrębnianie podciągów jest jeszcze prostsze dzięki użyciu wbudowanych funkcji bibliotecznych. Alternatywnym sposobem na wyodrębnianie podciągów jest użycie wyrażeń regularnych, jednak wymaga to użycia dedykowanych bibliotek lub narzędzi zewnętrznych.

Implementacja wyodrębniania podciągów wykorzystuje algorytmy porównywania ciągów znaków, na przykład Knuth-Morris-Pratt lub Boyer-Moore, aby znaleźć wzorzec podciągu w tekście. Osoby zainteresowane tematem mogą zgłębić szczegóły tych algorytmów i eksperymentować z własnymi implementacjami.

## Zobacz również:
Dla lepszego zrozumienia wyodrębniania podciągów w C, warto zapoznać się z dokumentacją funkcji bibliotecznych takich jak `strstr` czy `strncpy`. Można również przejrzeć przykładowe implementacje algorytmów porównywania ciągów znaków, aby głębiej zrozumieć jak wyodrębnianie podciągów jest realizowane w praktyce.