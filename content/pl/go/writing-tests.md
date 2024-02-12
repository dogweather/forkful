---
title:                "Pisanie testów"
aliases:
- pl/go/writing-tests.md
date:                  2024-02-03T18:15:25.919598-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie testów"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów w Go polega na tworzeniu małych, zarządzalnych fragmentów kodu, które weryfikują funkcjonalność i zachowanie aplikacji. Programiści piszą testy, aby upewnić się, że ich kod działa zgodnie z oczekiwaniami w różnych warunkach, ułatwić refaktoryzację oraz pomóc zapobiegać regresji.

## Jak to zrobić:

W Go testy są zazwyczaj pisane w tym samym pakiecie co kod, który testują. Pliki zawierające testy nazwane są z sufiksem `_test.go`. Testy to funkcje, które przyjmują wskaźnik do obiektu testing.T (z pakietu `testing`) jako argument i sygnalizują niepowodzenie, wywołując metody takie jak `t.Fail()`, `t.Errorf()` itp.

Przykład prostego testu dla funkcji `Add` zdefiniowanej w `math.go`:
```go
// math.go
package matematyka

func Add(x, y int) int {
    return x + y
}
```

Plik testowy `math_test.go`:
```go
package matematyka

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; chciano %d", result, expected)
    }
}
```

Uruchom swoje testy poleceniem `go test` w tym samym katalogu co Twoje pliki testowe. Przykładowe wyjście wskazujące na zdany test wyglądałoby podobnie do:

```
PASS
ok      example.com/my/math 0.002s
```

Dla testów sterowanych tabelą, które pozwalają na efektywne testowanie różnych kombinacji danych wejściowych i wyjściowych, zdefiniuj tablicę struktur reprezentujących przypadki testowe:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, function(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("otrzymano %d, oczekiwano %d", ans, tt.expected)
            }
        })
    }
}
```

## Dogłębna analiza

Framework testowy Go, wprowadzony w Go 1 razem z samym językiem, został zaprojektowany w celu bezproblemowej integracji z narzędziowiem Go, odzwierciedlając nacisk Go na prostotę i efektywność w rozwoju oprogramowania. W przeciwieństwie do niektórych frameworków testowych w innych językach, które polegają na zewnętrznych bibliotekach lub skomplikowanych konfiguracjach, wbudowany pakiet `testing` Go zapewnia prosty sposób na pisanie i uruchamianie testów.

Interesującym aspektem podejścia Go do testowania jest zasada konwencji ponad konfiguracją, którą przyjmuje, tak jak wzorzec nazewnictwa plików (`_test.go`) i używanie funkcjonalności biblioteki standardowej zamiast zewnętrznych zależności. To minimalizujące podejście zachęca programistów do pisania testów, ponieważ bariery wejścia są niskie.

Chociaż wbudowane narzędzia testowe Go pokrywają szeroki zakres, są scenariusze, w których narzędzia lub frameworki stron trzecich mogą oferować więcej funkcjonalności, takie jak generowanie atrap, testowanie fuzzowe czy testy w stylu Behavior-Driven Development (BDD). Popularne biblioteki takie jak Testify czy GoMock uzupełniają standardowe możliwości testowania w Go, oferując bardziej wyraziste asercje czy możliwości generowania atrap, które mogą być szczególnie przydatne w skomplikowanych aplikacjach z wieloma zależnościami.

Pomimo istnienia tych alternatyw, standardowy pakiet testowy Go pozostaje kamieniem węgielnym testowania w Go ze względu na jego prostotę, wydajność i ścisłą integrację z językiem oraz narzędziowiem. Niezależnie od tego, czy programiści zdecydują się na uzupełnienie go narzędziami stron trzecich, czy nie, framework testowy Go zapewnia solidną podstawę do zapewniania jakości i niezawodności kodu.
