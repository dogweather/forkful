---
title:                "Go: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy w języku Go?

Testowanie kodu jest nieodzownym elementem tworzenia wysokiej jakości oprogramowania. Pisanie testów w języku Go jest nie tylko ważne, ale również bardzo proste i daje wiele korzyści. Dzięki odpowiedniej praktyce testowej, możemy mieć pewność, że nasz kod działa poprawnie i w przypadku wprowadzenia zmian, nie wpłynie to negatywnie na funkcjonowanie aplikacji.

## Jak napisać testy w języku Go?

```Go
// Przykładowy kod zawierający funkcję wyznaczającą sumę dwóch liczb
func suma(a int, b int) int {
    return a + b
}
```

Aby napisać test dla powyższej funkcji, musimy zaimportować pakiet *testing* i stworzyć funkcję testującą z prefiksem *Test* i parametrem *t *testing.T*.

```Go
import "testing"

func TestSuma(t *testing.T) {
    // Przygotowanie danych do testu
    a := 5
    b := 10
    expected := 15

    // Wywołanie funkcji, którą chcemy przetestować
    result := suma(a, b)

    // Porównanie otrzymanego wyniku z oczekiwanym
    if result != expected {
        // W przypadku błędu, zgłaszamy błąd testu
        t.Errorf("Suma(%d, %d) = %d; oczekiwano %d", a, b, result, expected)
    }
}
```

Wynik wywołania funkcji *go test* powinien być następujący:

```
--- FAIL: TestSuma (0.00s)
    main_test.go:15: Suma(5, 10) = 11; oczekiwano 15
FAIL
```

## Głębsze zagadnienia związane z pisaniem testów w języku Go

Pisanie dobrych testów wymaga od nas odpowiedniego podejścia i zapoznania się z mechanizmami dostępnymi w języku Go. Jednym z kluczowych elementów jest wykorzystanie asercji, czyli wyrażeń lub funkcji, które pozwalają nam porównać oczekiwany wynik z rzeczywistym.

Ponadto, ważne jest również testowanie różnych przypadków oraz sprawdzanie wyjątków. W języku Go możemy to osiągnąć wykorzystując funkcje *t.Run()* oraz *t.Skip()*.

## Zobacz również!

* Dokumentacja języka Go na temat testowania: https://golang.org/pkg/testing/
* Przykłady testów w języku Go: https://github.com/golang/go/wiki/LearnTesting
* Artykuł na blogu Go: https://blog.golang.org/cover