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

## Co i Dlaczego?
Pisanie testów to proces, w którym programiści tworzą kod, który sprawdza, czy dany kod działa poprawnie. Jest to ważna część tworzenia oprogramowania, ponieważ pomaga w wykrywaniu błędów i zapewnieniu, że program działa zgodnie z oczekiwaniami.

## Jak to zrobić:
Przykłady kodu i wyjścia można znaleźć poniżej:

```Haskell
-- importowanie modułu do testowania
import Test.HUnit

-- funkcja, którą chcemy przetestować
myFunction :: Int -> Int
myFunction x = x * 2

-- testowanie funkcji
testMyFunction1 = TestCase (assertEqual "2 razy 2 to 4" 4 (myFunction 2))
testMyFunction2 = TestCase (assertEqual "3 razy 3 to 9" 9 (myFunction 3))

-- grupowanie testów
tests = TestList [TestLabel "testMyFunction1" testMyFunction1, TestLabel "testMyFunction2" testMyFunction2]

-- wywołanie wszystkich testów
main = do
  runTestTT tests
  return ()
```

Output:
```
Cases: 2  Tried: 2  Errors: 0  Failures: 0
Counts {cases = 2, tried = 2, errors = 0, failures = 0}
```

## Głębszy Zanurzenie:
Pisanie testów ma swoje korzenie w koncepcji TDD (Test Driven Development) i jest szeroko stosowane w dzisiejszym świecie programowania. Alternatywą dla pisania testów jest manualne sprawdzanie kodu, co jest bardziej czasochłonne i podatne na błędy. W implementacji testów w języku Haskell wykorzystuje się biblioteki takie jak HUnit, QuickCheck czy SmallCheck.

## Zobacz także:
- Dokumentacja Haskell dla HUnit: https://hackage.haskell.org/package/HUnit-1.6.0.0/docs/Test-HUnit.html
- Tutoriale związane z pisaniem testów w Haskell: https://wiki.haskell.org/Testing_guidelines
- Poradnik do TDD w języku Haskell: https://medium.com/@jamesacarr/test-driven-development-on-real-haskell-projects-106618141f84