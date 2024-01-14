---
title:                "Haskell: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Testowanie jest kluczowym elementem w programowaniu, ponieważ pozwala nam upewnić się, że nasz kod działa zgodnie z oczekiwaniami. Dzięki testom możemy również zapewnić niezawodność i jakość naszego kodu.

## Jak to zrobić

Pierwszym krokiem w tworzeniu testów jest zaimportowanie modułu `Test.HUnit`:

```Haskell
import Test.HUnit
```

Następnie możemy zacząć pisać nasze testy wykorzystując funkcję `TestCase`, która ma dwa parametry: nazwę testu i sam test.

```Haskell
test1 = TestCase "Dodawanie liczb całkowitych" $ assertEqual "Wynik powinien być równy 10" (2+3) 5
```

Następnie możemy utworzyć listę zawierającą wszystkie nasze testy i wywołać funkcję `runTestTT`, aby uruchomić je wszystkie:

```Haskell
testy = [test1, test2, test3]
runTestTT (TestList testy)
```

Jeśli nasze testy przejdą, otrzymamy komunikat `OK`, jeśli nie otrzymamy informacji o niepowodzeniu.

## Dogłębna analiza

Ważne jest, aby pisać testy, które pokrywają wszystkie możliwe scenariusze. Możemy również wykorzystać moduł `QuickCheck` do testowania dowolnych danych wejściowych. Aby skorzystać z `QuickCheck`, musimy najpierw zaimportować odpowiedni moduł:

```Haskell
import Test.QuickCheck
```

Następnie definiujemy funkcję testującą z wykorzystaniem `quickCheck`:

```Haskell
sprawdzDzialanie :: Int -> Int -> Bool
sprawdzDzialanie x y = x * y == y * x
```

Teraz możemy wywołać `quickCheck` na naszej funkcji, aby przetestować ją z losowo wybranymi parametrami i sprawdzić, czy zawsze zwraca ona oczekiwany wynik:

```Haskell
quickCheck sprawdzDzialanie
```

## Zobacz też

- [Dokumentacja modułu `Test.HUnit`](https://hackage.haskell.org/package/HUnit)
- [Dokumentacja modułu `Test.QuickCheck`](https://hackage.haskell.org/package/QuickCheck)