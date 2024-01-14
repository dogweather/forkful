---
title:                "Haskell: Pisanie testów"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie testów jest nieodłączną częścią procesu tworzenia oprogramowania. Pozwala ono sprawdzić czy kod działa poprawnie, weryfikuje jego funkcjonalności oraz pozwala na szybkie wykrycie ewentualnych błędów. Jest to nie tylko narzędzie ułatwiające pracę programistów, ale również zapewnia bezpieczeństwo i jakość produktu końcowego.

## Jak to zrobić

Sprawne i efektywne pisanie testów w Haskellu może wydawać się trudne, ale w rzeczywistości jest to prostsze niż się wydaje. Właśnie dlatego postanowiliśmy stworzyć ten krótki poradnik, który pomoże Ci wdrożyć testy do swojego projektu.

```Haskell
-- Przykładowy test funkcji dodawania
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers a b = a + b

main :: IO ()
main = do
  -- Wywołanie funkcji addTwoNumbers z parametrami 2 i 3
  let result = addTwoNumbers 2 3
  -- Sprawdzenie czy wynik jest równy oczekiwanemu
  if result == 5
    then putStrLn "Test zakończony pomyślnie!"
    else putStrLn "Test nie powiódł się."
```

W powyższym przykładzie widzimy jak w prosty sposób tworzyć testy w Haskellu. Korzystając z funkcji `main` oraz konstrukcji `if-else`, możemy weryfikować czy nasze funkcje zwracają poprawne wyniki.

## Deep Dive

Aby pisać jeszcze lepsze testy w Haskellu, warto poznać niektóre z dostępnych bibliotek, takich jak HUnit czy QuickCheck. Biblioteka HUnit pozwala na pisanie testów jednostkowych, gdzie z kolei QuickCheck pozwala na generowanie i testowanie danych. Warto również zaznajomić się z pojęciem TDD (Test Driven Development), czyli tworzenia testów przed implementacją kodu.

## Zobacz także

- [Poradnik dla początkujących w języku Haskell](https://www.tomaszdrazil.com.pl/tworzenie-gry-w-haskellu-wstep/)
- [Dokumentacja biblioteki HUnit](https://hackage.haskell.org/package/HUnit)
- [Dokumentacja biblioteki QuickCheck](https://hackage.haskell.org/package/QuickCheck)