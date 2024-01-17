---
title:                "Pisanie testów"
html_title:           "Go: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-tests.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Pisanie testów jest często nieodłączną częścią procesu programowania. Polega to na tworzeniu małych fragmentów kodu, które sprawdzają, czy pozostałe elementy programu funkcjonują zgodnie z oczekiwaniami. Programiści piszą testy, aby upewnić się, że ich kod jest poprawny i wykona się bez problemów.

## Jak to zrobić:

```Go
// Zdefiniuj funkcję, dla której chcemy napisać testy
func reverse(str string) string {
    runes := []rune(str)
    for i, j := 0, len(runes)-1; i < len(runes)/2; i, j = i+1, j-1 {
        runes[i], runes[j] = runes[j], runes[i]
    }
    return string(runes)
}

// Importujemy pakiet "testing" do naszego kodu
import "testing"

// Definiujemy test funkcji reverse(), nadając mu nazwę TestReverse
func TestReverse(t *testing.T) {
    actual := reverse("Hello World")
    expected := "dlroW olleH"
    // Jeśli wynik jest niezgodny z oczekiwaniami, zgłaszamy błąd
    if actual != expected {
        t.Errorf("Got %v, want %v", actual, expected)
    }
}

// Uruchamiamy testy
go test
```

Wyjście powinno wyglądać następująco:

```Go
--- FAIL: TestReverse (0.00s)
	testing.go:173: Got dlroW olleH, want Hello World
FAIL
exit status 1
FAIL    /path/to/your/code/package 0.012s
```

## Zagłębienie:

Pisanie testów jest ważną częścią tzw. "test-driven development", czyli programowania wyznaczanego przez testy. Praktyka ta promuje tworzenie testów przed faktycznym pisaniem kodu. Istnieją również inne metodyki, takie jak "behaviour-driven development", w których testy są bardziej opisowe niż techniczne.

Główną alternatywą dla pakietu "testing" jest narzędzie "ginkgo", które oferuje lepszą czytelność i więcej funkcjonalności. Jednak pakiet "testing" jest częścią standardowej biblioteki języka Go i jest łatwo dostępny do użycia.

## Zobacz również:

- [Testing in Go] (https://golang.org/pkg/testing/)
- [Ginkgo] (https://github.com/onsi/ginkgo)
- [Test-Driven Development] (https://pl.wikipedia.org/wiki/Test-driven_development)