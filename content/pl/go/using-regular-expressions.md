---
title:                "Używanie wyrażeń regularnych"
html_title:           "Go: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Używanie wyrażeń regularnych jest jednym z narzędzi, które programiści używają do przetwarzania i manipulacji tekstu. Może to być przydatne w przypadku wstępnego przetwarzania danych lub w wyszukiwaniu określonych wzorców w tekście. Programiści często korzystają z wyrażeń regularnych, aby przyspieszyć swoją pracę i upewnić się, że ich kod jest dokładny i precyzyjny.

## Jak to zrobić:
```Go
// Przykładowy kod w języku Go wykorzystujący wyrażenia regularne
package main
import (
    "fmt"
    "regexp"
)

func main() {
    // Tworzenie wyrażenia regularnego
    pattern := regexp.MustCompile("(?i)hello")

    // Sprawdzanie, czy wyrażenie pasuje do tekstu
    fmt.Println(pattern.MatchString("Hello world!")) // Output: true
    
    // Wyszukiwanie wszystkich wystąpień wyrażenia w tekście
    fmt.Println(pattern.FindAllString("Hello my friend, hello again!", -1)) // Output: [Hello, hello]

    // Zastępowanie wyrażenia w tekście innym wyrażeniem lub stałą
    fmt.Println(pattern.ReplaceAllString("Hello Go!", "Hi")) // Output: Hi Go! 
}
```

## Głębsza analiza:
Wyrażenia regularne zostały wprowadzone w 1951 roku przez Stephena Cole Kleene'a jako środek do opisywania wzorców w językach formalnych. W dzisiejszych czasach jest to powszechnie używane narzędzie w wielu językach programowania, a także w aplikacjach internetowych i edytorach tekstu. Alternatywami dla wyrażeń regularnych są m.in. parser-generatory oraz biblioteki tokenizacyjne. W języku Go wyrażenia regularne są implementowane przez pakiet "regexp" i korzystają z wyrażeń zwyczajnych lub składni PCRE.

## Zobacz również:
- Oficjalna dokumentacja wyrażeń regularnych w języku Go: https://golang.org/pkg/regexp/
- Przewodnik po wyrażeniach regularnych w języku Go: https://github.com/siongui/userpages/wiki/regexp
- Narzędzie online do testowania wyrażeń regularnych w języku Go: https://regex-golang.appspot.com/
- Przydatny artykuł na temat wyrażeń regularnych w języku Go: https://blog.golang.org/regular-go