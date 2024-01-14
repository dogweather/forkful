---
title:    "Haskell: Pisanie testów"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Testy są nieodłączną częścią procesu tworzenia oprogramowania. Pisząc testy, możemy mieć pewność, że nasz kod działa zgodnie z oczekiwaniami i nie wprowadza niepożądanych błędów. Dodatkowo, testowanie pozwala nam szybko wykryć i naprawić ewentualne problemy, co zwiększa jakość i niezawodność naszego kodu.

## Jak

Najważniejszą częścią pisania testów jest wykorzystanie modułu `Test.HUnit` lub `Test.Hspec`, które są częścią popularnego w Haskellu frameworka do testowania o nazwie `hspec`. Aby rozpocząć, musimy zaimportować odpowiedni moduł, a następnie odpowiednio zdefiniować testy.

Przykładowy test z wykorzystaniem `Test.HUnit` wyglądałby następująco:

```Haskell
import Test.HUnit

-- definicja funkcji, którą będziemy testować
double :: Int -> Int
double x = x * 2

-- funkcja do testowania
testDouble :: Test
testDouble = TestCase $ assertEqual "double 2 should be 4" 4 (double 2)

-- lista testów, którą przekażemy do funkcji runTest
tests :: Test
tests = TestList [testDouble]

main :: IO ()
main = runTestTT tests
```

Wywołanie funkcji `runTestTT` spowoduje uruchomienie wszystkich testów zdefiniowanych w liście `tests` i wyświetlenie ich wyników w konsoli. Jeśli wszystkie testy zostaną zakończone succesem, otrzymamy taki wynik:

```Haskell
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

Przykład z użyciem `hspec` wyglądałby trochę inaczej, ale również widać w nim wykorzystanie modułu `Test.HUnit`:

```Haskell
import Test.Hspec

-- definicja funkcji, którą będziemy testować
double :: Int -> Int
double x = x * 2

-- funkcja do testowania
main :: IO ()
main = hspec $ do
  describe "double" $ do
    it "should return double of input" $
      double 2 `shouldBe` 4
```

Funkcja `hspec` przyjmuje jako argument funkcję, która używa funkcji `describe` do grupowania testów i funkcji `it` do definiowania konkretnych testów.

## Deep Dive

Pisanie testów to sztuka, która wymaga praktyki i umiejętności. Oto kilka wskazówek, które mogą pomóc Ci pisać skuteczne i czytelne testy w Haskellu:

- Warto definiować nazwy testów w sposób opisowy, aby łatwo było zrozumieć, co jest testowane i jakie oczekiwane są wyniki (np. `testDoublePositiveNumber`, `testSquareRootOfNegativeNumber`).
- Wykorzystaj funkcje `assertBool` i `assertString` do sprawdzania warunków logicznych w testach.
- W przypadku testów wykorzystujących IO (np. testy funkcji obsługujących pliki) użyj funkcji `runIO` do uruchomienia opakowanego kodu IO wewnątrz testu.
- Stwórz zmienną lub funkcję, która będzie przechowywać wspólne dane dla wszystkich testów, aby uniknąć nadmiernego powtarzania kodu.
- Korzystaj z funkcji `shouldThrow` lub `shouldReturn` wraz z `hspec` do łatwego sprawdzania wyjątków i wyników funkcji.

## Zobacz także

- [Dokumentacja modułu Test.HUnit](https://hackage.haskell.org/package/HUnit-1.6.0.0/docs/Test-HUnit.html)
- [Dokument