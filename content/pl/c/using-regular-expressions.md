---
title:    "C: Użycie wyrażeń regularnych"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego?

Regularne wyrażenia są potężnym narzędziem w programowaniu C, które pozwalają na sprawdzenie i manipulowanie tekstem w sposób precyzyjny. Dzięki nim można szybko i skutecznie przetwarzać dane, a także wykonywać złożone operacje na tekście.

## Jak to zrobić?

Aby używać regularnych wyrażeń w języku C, najpierw musimy zaimportować bibliotekę <regex.h>. Poniżej znajduje się prosty przykład pokazujący, jak użyć wyrażenia regularnego do znalezienia wszystkich liczb w tekście:

```C
#include <stdio.h>
#include <regex.h>
 
int main()
{
    regex_t regex; // deklaracja zmiennej typu regex
    int result; // deklaracja zmiennej przechowującej wynik
    char text[] = "Witaj, mam na imię Jan i urodziłem się w 1985 roku."; // przykładowy tekst
    char pattern[] = "[0-9]+"; // wyrażenie regularne wyszukujące liczby
 
    result = regcomp(&regex, pattern, 0); // kompilacja wyrażenia regularnego
    if (result) {
        printf("Błąd kompilacji wyrażenia regularnego\n");
        return 1;
    }
 
    // użycie funkcji regexec do wyszukania pasujących fragmentów
    result = regexec(&regex, text, 0, NULL, 0);
    if (!result) {
        printf("Podałeś swoje urodziny w tekście!\n");
    }
    
    regfree(&regex); // zwolnienie pamięci
    return 0;
}
```

Powyższy kod wyświetli na ekranie komunikat "Podałeś swoje urodziny w tekście!", ponieważ wyrażenie regularne zostało dopasowane do liczby 1985 w tekście.

## Wnikliwszy ogląd

Wyrażenia regularne w języku C są oparte na standardzie IEEE 1003.1, który definiuje ich składnię i funkcje. Do składni należą m.in. specjalne znaki, które określają reguły dopasowania, takie jak "^" (znak początku tekstu) i "$" (znak końca tekstu). Ponadto, funkcje takie jak *regcomp* i *regexec* służą do kompilowania i wykonywania wyrażeń regularnych.

Możemy również wykorzystać grupy w wyrażeniu regularnym, aby zawęzić zakres wyszukiwanych danych. Na przykład, "[A-Z]+[0-9]+" dopasuje dowolną sekwencję dużych liter, a następnie dowolną sekwencję cyfr.

Powyższe przykłady tylko zaczynają zgłębiać temat regularnych wyrażeń w programowaniu C. Istnieje wiele więcej zaawansowanych funkcji i opcji, które są dostępne do wykorzystania. Dlatego zachęcamy do pogłębiania swojej wiedzy na ten temat poprzez dokumentację i inne źródła.

## Zobacz także

- Dokumentacja regularnych wyrażeń w języku C: https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
- Przykładowe zastosowania regularnych wyrażeń w programowaniu C: https://www.geeksforgeeks.org/regular-expressions-in-c/
- Kurs online z wykorzystaniem regularnych wyrażeń w programowaniu C: https://www.cprogramming.com/tutorial/regular-expressions-c.html