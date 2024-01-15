---
title:                "Pisanie testów"
html_title:           "Haskell: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy w Haskellu?

Czasy, kiedy programiści uważali testowanie za zbędny luksus, już dawno minęły. W dzisiejszych czasach, kiedy projekty są coraz większe i bardziej skomplikowane, testy stają się niezbędną częścią tworzenia oprogramowania. W przypadku języka Haskell, który promuje funkcjonalność, pisanie testów jest szczególnie ważne. Pozwala to zapewnić, że nasze funkcje działają zgodnie z oczekiwaniami i nie występują w nich niepożądane efekty uboczne.

## Jak napisać testy w Haskellu?

Aby napisać testy w Haskellu, możesz skorzystać z biblioteki HUnit. Najpierw, musisz zaimportować ją do swojego pliku:

```
import Test.HUnit
```

Następnie, możesz definiować testy przy użyciu funkcji `TestList`. Przykładowy test może wyglądać tak:

```
test1 = TestCase (assertEqual "Dodawanie do siebie dwóch liczb" (2+2) 4)
```

Aby uruchomić ten test, musisz użyć funkcji `runTestTT` w głównej funkcji `main`:

```
main = runTestTT test1
```

Jeśli test się nie powiedzie, zobaczysz odpowiednie komunikaty w konsoli. Aby przetestować wiele przypadków, możesz użyć funkcji `TestLabel` do nazwania każdego testu i umieścić je w liście:

```
tests = TestList [TestLabel "Test dodawania" test1, TestLabel "Test odejmowania" test2]
```

## Dogłębne zagłębienie

Podczas pisania testów w Haskellu, warto pamiętać o kilku ważnych aspektach. Po pierwsze, testy powinny być proste i łatwe do zrozumienia. W przeciwnym razie, zmiana testów może stać się koszmarem. Po drugie, testy powinny być niezależne od siebie. Nie powinno być zależności pomiędzy testami, ponieważ może to prowadzić do błędów i pomyłek. Oznacza to, że każdy test powinien testować tylko jedną funkcję lub pojedynczy scenariusz.

Warto również pamiętać o wyjątkach i niepożądanych efektach ubocznych. Testowanie funkcji, które wywołują inne funkcje lub mają niezależne efekty uboczne jeszcze bardziej zwiększa potrzebę pisania testów.

## Zobacz również

- [Documentation for HUnit](https://hackage.haskell.org/package/HUnit)
- [Haskell School of Expression](https://www.cs.yale.edu/homes/hudak/SOE/)
- [10 Minute Test-Driven Development in Haskell](https://medium.com/@dinosanciagopardo/test-driven-haskell-a-how-to-2d10cabba19e)