---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:17.717139-07:00
description: "Jak to zrobi\u0107: Aby u\u017Cywa\u0107 wyra\u017Ce\u0144 regularnych\
  \ w C, b\u0119dziesz przede wszystkim pracowa\u0107 z bibliotek\u0105 POSIX regex\
  \ (`<regex.h>`). Ten przyk\u0142ad demonstruje\u2026"
lastmod: '2024-03-13T22:44:35.874978-06:00'
model: gpt-4-0125-preview
summary: "Aby u\u017Cywa\u0107 wyra\u017Ce\u0144 regularnych w C, b\u0119dziesz przede\
  \ wszystkim pracowa\u0107 z bibliotek\u0105 POSIX regex (`<regex.h>`)."
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
weight: 11
---

## Jak to zrobić:
Aby używać wyrażeń regularnych w C, będziesz przede wszystkim pracować z biblioteką POSIX regex (`<regex.h>`). Ten przykład demonstruje podstawowe dopasowywanie wzorców:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // Wzorzec do dopasowania ciągów rozpoczynających się od 'a' po którym następują znaki alfanumeryczne
    char *test_string = "apple123";

    // Kompilacja wyrażenia regularnego
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Nie udało się skompilować wyrażenia regularnego\n");
        exit(1);
    }

    // Wykonanie wyrażenia regularnego
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Znaleziono dopasowanie\n");
    } else if (return_value == REG_NOMATCH) {
        printf("Nie znaleziono dopasowania\n");
    } else {
        printf("Dopasowanie wyrażenia regularnego nie powiodło się\n");
        exit(1);
    }

    // Zwolnienie pamięci przydzielonej dla wyrażenia regularnego
    regfree(&regex);

    return 0;
}
```

Przykładowe wyjście dla ciągu pasującego ("apple123"):
```
Znaleziono dopasowanie
```
I dla ciągu niepasującego ("banana"):
```
Nie znaleziono dopasowania
```

## Szczegółowa analiza:
Wyrażenia regularne w C, jako część standardu POSIX, oferują solidny sposób na wykonywanie dopasowań i manipulacji ciągami znaków. Jednak API biblioteki POSIX regex w C jest uznawane za bardziej żmudne niż te, które można znaleźć w językach zaprojektowanych z myślą o zaawansowanej manipulacji ciągami znaków, takich jak Python czy Perl. Składnia wzorców jest podobna w różnych językach, ale C wymaga ręcznego zarządzania pamięcią i większej ilości kodu szablonowego, aby przygotować, wykonać i posprzątać po użyciu wzorców regex.

Pomimo tych wyzwań, nauka użycia regex w C jest nagradzająca, ponieważ pogłębia zrozumienie koncepcji programowania na niższym poziomie. Dodatkowo, otwiera to możliwości dla programowania w C w obszarach takich jak przetwarzanie tekstu i ekstrakcja danych, gdzie regex jest niezastąpiony. Dla bardziej skomplikowanych wzorców lub operacji regex, alternatywy takie jak biblioteka PCRE (Perl Compatible Regular Expressions) mogą zaoferować bardziej funkcjonalne i do pewnego stopnia łatwiejsze interfejs, chociaż wymaga to integracji zewnętrznej biblioteki z projektem w C.
