---
title:                "Go: Używanie wyrażeń regularnych"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions (wyrażenia regularne) są niezbędnym narzędziem dla każdego programisty Go. Pozwalają one na wyszukiwanie i manipulowanie tekstem w bardzo skuteczny sposób. Jeśli chcesz poznać szybkie i wydajne sposoby na przetwarzanie danych tekstowych, wyrażenia regularne są dla Ciebie niezbędnym narzędziem.

## Jak to zrobić

Aby rozpocząć korzystanie z wyrażeń regularnych w Go, musimy najpierw zaimportować pakiet "regexp". Następnie możemy użyć funkcji "MatchString" lub "FindString" w celu przeprowadzenia wyszukiwania w danym tekście. Poniższy przykład pokazuje, jak znaleźć wszystkie wystąpienia słowa "Go" w tekście:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go jest świetnym językiem programowania, którego warto się nauczyć."

    // Utworzenie wyrażenia regularnego pasującego do słowa "Go"
    pattern := regexp.MustCompile("Go")

    // Wykorzystanie funkcji FindString do znalezienia pierwszego pasującego fragmentu tekstu
    match := pattern.FindString(text)
    fmt.Println(match) // Output: "Go"

    // Wykorzystanie funkcji MatchString do sprawdzenia, czy w tekście znajduje się pasujące wyrażenie
    isMatch := pattern.MatchString(text)
    fmt.Println(isMatch) // Output: true

    // Wykorzystanie funkcji FindAllString do znalezienia wszystkich pasujących fragmentów tekstu
    allMatches := pattern.FindAllString(text, -1)
    fmt.Println(allMatches) // Output: ["Go", "Go"]
}
```

## Głęboki zanurzenie

Wyrażenia regularne są bardzo wszechstronnym narzędziem, a ich pełne wykorzystanie wymaga zgłębienia większej ilości informacji. Możliwe jest definiowanie bardziej skomplikowanych wzorców za pomocą operatorów kwantyfikatorów, grupowania i wyrażeń warunkowych. W Go istnieje również możliwość wykorzystania wyrażeń regularnych do podstawienia lub zamiany tekstu.

Poniższe linki prowadzą do różnych zasobów, które mogą pomóc Ci lepiej zrozumieć wyrażenia regularne w Go i ich zastosowania:

## Zobacz też

- [Dokumentacja pakietu "regexp" w języku Go](https://golang.org/pkg/regexp/)
- [Kurs na temat wyrażeń regularnych w Go na stronie learn-golang.org](https://learn-golang.org/regex)
- [Wyrażenia regularne online - narzędzie do testowania wyrażeń regularnych w różnych językach programowania](https://regex101.com/)