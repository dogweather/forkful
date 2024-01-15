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

## Dlaczego

Zastanawiasz się, dlaczego warto pisać testy w Go? To prostsze niż myślisz! Testy są niezwykle ważne w procesie tworzenia oprogramowania, ponieważ pozwalają upewnić się, że kod działa poprawnie i jest odporny na błędy.

## Jak To Zrobić

Aby napisać testy w Go, musisz użyć wbudowanego narzędzia "testing", które oferuje wiele przydatnych funkcji. Możesz zdefiniować testy za pomocą specjalnych funkcji, takich jak `func TestXxx(t *testing.T)`, gdzie Xxx to nazwa testowanej funkcji. Wewnątrz tej funkcji możesz użyć asercji, aby sprawdzić, czy otrzymane wyniki są zgodne z oczekiwaniami. Oto przykładowy kod:

```Go
func TestSum(t *testing.T) {
    total := sum(2, 3)
    expected := 5
    if total != expected {
        t.Errorf("Sum of %d and %d expected to be %d, but got %d", 2, 3, expected, total)
    }
}
```

Powyższy test sprawdza, czy funkcja `sum()` zwraca poprawny wynik dla podanych argumentów. Jeśli wynik nie jest zgodny z oczekiwaniami, test jest oznaczony jako nieudany i zostaje wyświetlony błąd. Możesz również użyć funkcji `t.Log()` aby wyświetlić dodatkowe informacje dla błędów.

## Deep Dive

Pisanie testów w Go nie musi być trudne. Musisz tylko pamiętać o kilku ważnych rzeczach:

- Nazwy testów muszą zaczynać się od słowa "Test", aby były wykryte przez narzędzie `testing`.
- Asercje powinny być umieszczone wewnątrz funkcji `func TestXxx(t *testing.T)` i powinny używać metody `t.Errorf()` do raportowania błędów.
- Możesz użyć funkcji `t.Run()` do grupowania testów i wyświetlania bardziej szczegółowych informacji o nieudanych testach.

Należy również pamiętać, że pisanie testów to tylko część procesu. Ważne jest również uruchamianie tych testów i sprawdzanie ich wyników regularnie. W tym celu możesz wykorzystać narzędzia takie jak Continous Integration (CI), które automatycznie uruchamiają testy i informują o wynikach.

## Zobacz również

- [Dokumentacja oficjalna Go o testowaniu](https://golang.org/pkg/testing/)
- [Artykuł na Medium o pisaniu testów w Go](https://medium.com/@matryer/5-simple-tips-and-tricks-for-writing-unit-tests-in-golang-619653f90742)
- [Wideo na Youtube o testowaniu w Go](https://www.youtube.com/watch?v=ndmB0bj7eyw)