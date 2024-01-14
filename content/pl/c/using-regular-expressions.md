---
title:    "C: Używanie wyrażeń regularnych"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Regularne wyrażenia są nieodłączną częścią wielu języków programowania, w tym także języka C. Są to niezwykle przydatne narzędzia do przetwarzania i manipulowania tekstem. Korzystając z regularnych wyrażeń, możemy szybko i łatwo wyszukiwać wzorce w tekście, co znacznie ułatwia pracę z danymi i zwiększa wydajność naszego kodu.

## Jak to zrobić

Aby skorzystać z regularnych wyrażeń w języku C, musimy użyć biblioteki "regex.h". W pierwszym kroku należy zadeklarować zmienną typu "regex_t", która będzie przechowywać wyrażenie regularne. Następnie, za pomocą funkcji "regcomp" możemy skompilować nasze wyrażenie i przypisać je do naszej zmiennej. Wielką zaletą korzystania z biblioteki "regex.h" jest możliwość użycia funkcji "regexec", która zwraca informację o tym czy nasz wzorzec występuje w tekście czy nie. W przypadku gdy wzorzec został znaleziony, możemy też uzyskać informację o dokładnym położeniu wzorca w tekście.

```C
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main() {
    char *text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
    regex_t regex;
    int result;
    
    // kompilacja wyrażenia regularnego
    result = regcomp(&regex, "dolor", 0);
    
    if (result != 0) 
        fprintf(stderr, "Błąd kompilacji wyrażenia.\n");
    else {
        // sprawdzenie czy wzorzec występuje w tekście
        result = regexec(&regex, text, 0, NULL, 0);
        
        if (result == 0)
            printf("Wzorzec znaleziony w tekście.\n");
        else
            printf("Wzorzec nie został znaleziony.\n");
    }
    
    // zwolnienie pamięci
    regfree(&regex);
    
    return 0;
}
```

**Wyjście:**
Wzorzec znaleziony w tekście.

## Deep Dive

W wyrażeniach regularnych możemy wykorzystywać wiele różnych symboli i funkcji, co pozwala nam na tworzenie bardzo precyzyjnych wzorców. Oto kilka przykładów:

- "[a-z]" - oznacza każdą małą literę,
- "[A-Z]" - oznacza każdą wielką literę,
- "[0-9]" - oznacza każdą cyfrę,
- "[a-zA-Z0-9]" - oznacza każdą literę i cyfrę,
- "." - oznacza dowolny znak,
- "*" - oznacza dowolną liczbę powtórzeń,
- "+" - oznacza jedno lub więcej powtórzeń,
- "?" - oznacza zero lub jedno powtórzenie,
- "^" - oznacza początek tekstu,
- "$" - oznacza koniec tekstu.

Powyższe przykłady to tylko namiastka możliwości wyrażeń regularnych. Wszystkie dostępne symbole i funkcje można znaleźć w dokumentacji bibloteki "regex.h".

## Zobacz także

- Dokumentacja biblioteki "regex.h": https://www.gnu.org/software/gnulib/manual/html_node/GNU-Regex.html
- Przykładowe wyrażenia regularne w języku C: https://www.tutorialspoint.com/compile_c_online.php?PID=0Bw_CjBb95KQMOTZzU0JlVnNRX1E