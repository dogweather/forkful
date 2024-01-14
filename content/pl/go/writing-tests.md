---
title:                "Go: Pisanie testów"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-tests.md"
---

{{< edit_this_page >}}

# Dlaczego warto pisać testy w języku Go?

# Dlaczego

Testowanie kodu jest nieodłączną częścią procesu tworzenia oprogramowania. Pomaga zapewnić, że nasz kod działa poprawnie i spełnia wymagania. W języku Go, pisanie testów jest prostsze i bardziej efektywne dzięki wbudowanej bibliotece do testowania. Dlaczego warto więc poświęcić czas na pisanie testów w Go? Kolejne sekcje artykułu odpowiadają na to pytanie.

# Jak to zrobić?

Jeśli jesteś początkującym w języku Go, zacznij od nauki podstaw testowania. To pomoże Ci zrozumieć, jak testy są tworzone i jak działają. Poniżej znajdują się przykłady kodu w języku Go, które pokazują, jak pisać testy.

```Go
// Przykład testu jednostkowego dla funkcji dodawania
func TestSum(t *testing.T) {
    result := add(2, 3)
    expected := 5

    if result != expected {
        t.Errorf("Wynik jest niepoprawny, oczekiwano: %v, otrzymano: %v", expected, result)
    }
}
```

```Go
// Przykład testu integracyjnego dla serwisu HTTP
func TestHTTPService(t *testing.T) {
    req, err := http.NewRequest("GET", "/test", nil)
    if err != nil {
        t.Fatal("Nie udało się wykonać żądania HTTP:", err)
    }

    rr := httptest.NewRecorder()
    handler := http.HandlerFunc(testHandler)
    handler.ServeHTTP(rr, req)

    if status := rr.Code; status != http.StatusOK {
        t.Errorf("Niepoprawny kod odpowiedzi, oczekiwano: %v, otrzymano: %v", http.StatusOK, status)
    }

    expected := `{"message": "Testowy serwis HTTP działa poprawnie"}`
    if rr.Body.String() != expected {
        t.Errorf("Niepoprawny ciało odpowiedzi, oczekiwano: %v, otrzymano: %v", expected, rr.Body.String())
    }
}
```

W powyższych przykładach wykorzystujemy wbudowane funkcje w bibliotece do testowania języka Go, takie jak `t.Errorf` czy `t.Fatal`, aby informować o niepoprawnych wynikach testów.

# Deep Dive

Testowanie w języku Go jest oparte na konwencjach. Aby prawidłowo napisać test, należy zazwyczaj użyć prefiksu `Test`, aby nazwać funkcję, oraz przekazać parametr `*testing.T` do funkcji. Więcej informacji na temat pisania testów w języku Go można znaleźć w [dokumentacji officjalnej](https://golang.org/pkg/testing/).

Jedną z najważniejszych zalet pisania testów w języku Go jest możliwość równoległego uruchamiania testów. Dzięki temu, testy w Go są szybsze i wydajniejsze niż w niektórych innych językach.

# Zobacz także

- [Dokumentacja oficjalna języka Go](https://golang.org/doc/)
- [Tutorial o testowaniu w języku Go](https://golang.org/doc/code.html#Testing)
- [Blog o języku Go](https://blog.golang.org/)