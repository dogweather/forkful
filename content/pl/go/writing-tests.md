---
title:    "Go: Pisanie testów"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisanie testów jest ważne w programowaniu Go?

Testowanie jest nieodłączną częścią procesu tworzenia oprogramowania, a w języku Go jest to szczególnie istotne. Zapewnienie odpowiedniej jakości kodu jest kluczowe dla sukcesu projektu, a testy są ważnym narzędziem w uzyskaniu tego celu. Przeczytaj ten artykuł, aby dowiedzieć się dlaczego pisanie testów jest ważnym elementem pracy z językiem Go.

## Jak pisać testy w języku Go?

Pisanie testów w języku Go jest łatwe i przyjemne dzięki wbudowanemu w język pakietowi `testing`. Aby napisać test, wystarczy utworzyć nowy plik z sufiksem "_test.go" i użyć odpowiednich funkcji i metod z pakietu `testing`. Poniżej przedstawiamy przykładowy kod z komentarzami, który pokazuje jak napisać test jednostkowy w języku Go.

```Go
package main

import (
    "testing" // importowanie pakietu testing
)

// Funkcja testująca
func TestAddition(t *testing.T) {
    // Tworzenie testowych danych
    x := 5
    y := 10
    expected := 15 // oczekiwany wynik

    // Wywołanie funkcji do przetestowania
    result := Add(x,y)

    if result != expected {
        // W przypadku błędu wypisywanie informacji o teście
        t.Errorf("Dodawanie nie działa poprawnie. Oczekiwany wynik: %d, otrzymany wynik: %d", expected, result)
    }
}
```
Uruchomienie powyższego testu zwróci następujący wynik:
```
--- FAIL: TestAddition (0.00s)
    dodawanie_test.go:16: Dodawanie nie działa poprawnie. Oczekiwany wynik: 15, otrzymany wynik: 20
FAIL
```

Wynik ten wskazuje, że w teście wystąpił błąd, ponieważ oczekiwany wynik to 15, a otrzymany wynik to 20. W ten sposób możemy w łatwy sposób sprawdzić poprawność działania naszej funkcji.

## Deep Dive: Czym są testy jednostkowe i jakie są ich zalety?

Testy jednostkowe są jednym z rodzajów testów, które służą do sprawdzania poprawności kodu na poziomie pojedynczych funkcji lub modułów. Najważniejszą zaletą testów jednostkowych jest zdolność do szybkiego wykrywania błędów podczas pisania kodu. Dzięki nim można również łatwiej refaktorować kod, ponieważ dowiadujemy się, czy zmiany wprowadzone w kodzie nie wpłynęły negatywnie na działanie funkcji. Testy jednostkowe zapewniają też większą pewność w kwestii stabilności i wydajności aplikacji.

## Zobacz także

- [Dokumentacja pakietu testing w języku Go](https://golang.org/pkg/testing/)
- [Słów kilka o testowaniu w języku Go](https://medium.com/@johnnystarling/testing-in-go-a-quick-example-67fe23306d3c)
- [Blog o języku Go](https://blog.golang.org/)