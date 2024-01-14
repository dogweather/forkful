---
title:    "Haskell: Pisanie testów"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Odpowiednie testowanie kodu jest kluczowe dla stworzenia niezawodnych i wydajnych aplikacji. Dzięki testom, możemy upewnić się, że nasz kod działa zgodnie z oczekiwaniami i nie zawiera błędów, co pomaga w uniknięciu problemów w przyszłości.

## Jak to zrobić

Najpierw musimy zainstalować pakiet `HUnit`, który umożliwi nam pisanie testów w Haskellu. Następnie definiujemy nasze testy w funkcji `test` z wykorzystaniem funkcji `assertEqual`, która porównuje oczekiwany wynik z rzeczywistym wynikiem testowanej funkcji. W poniższym przykładzie, testujemy funkcję `double`, która podwaja podaną liczbę.

```Haskell
import Test.HUnit

double :: Int -> Int 
double x = x * 2

test = TestCase (assertEqual "Podwajanie liczby 2" 4 (double 2))
```

Aby uruchomić nasze testy, używamy funkcji `runTestTT` i przekazujemy do niej funkcję `test` jako argument. Po uruchomieniu, otrzymamy wynik testów w formacie `Counts`, który informuje nas o liczbie zaliczonych i niezaliczonych testów.

```Haskell
main = runTestTT test
```

Wynik dla powyższego kodu powinien wyglądać następująco:

```
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

## Wnikliwe spojrzenie

Aby napisać jeszcze bardziej skomplikowane testy, możemy sięgnąć po funkcje takie jak `assertBool`, `assertFailure` lub `assertString`, które pozwalają nam stworzyć testy na więcej niż jeden warunek. Możemy także tworzyć testy grupujące za pomocą funkcji `TestList`, co daje nam możliwość testowania większej ilości funkcji w jednym miejscu. Istnieją także inne narzędzia, takie jak `QuickCheck` czy `Hspec`, które oferują jeszcze więcej funkcji do testowania kodu w Haskellu.

## Zobacz również

- [Dokumentacja pakietu HUnit](https://hackage.haskell.org/package/HUnit)
- [Porównanie różnych narzędzi do testowania kodu w Haskellu](https://www.fpcomplete.com/blog/2017/09/unit-testing-in-haskell)
- [Oficjalny przewodnik po testowaniu w Haskellu](https://www.haskell.org/cabal/users-guide/#testing)