---
title:                "Pisanie do standardowego błędu"
html_title:           "Haskell: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie do standardowego wyjścia błędów to proces polegający na wyświetleniu wiadomości o błędzie lub ostrzeżenia w czasie wykonywania programu. Programiści często stosują to do debugowania i sprawdzania, czy ich program działa poprawnie.

## Jak to zrobić:
Kodowanie do standardowego wyjścia błędów w Haskellu jest proste, wystarczy użyć funkcji `hPutStrLn` z modułu `System.IO`. Przykładowy kod wygląda następująco:
```Haskell
import System.IO

main :: IO ()
main = do hPutStrLn stderr "Błąd: nieprawidłowy argument."
          putStrLn "Podaj argument: "
```
Powyższy kod wyświetli wiadomość o błędzie w konsoli, a następnie poprosi użytkownika o podanie argumentu.

## Wnikliwa analiza:
Pisanie do standardowego wyjścia błędów może być pomocne podczas debugowania programów, ponieważ dzięki temu możemy w łatwy sposób zlokalizować miejsca, w których występują błędy. Wcześniej, w starszych wersjach Haskell, pisano do standardowego wyjścia błędów za pomocą funkcji `fail`, jednak obecnie jest ona rzadko wykorzystywana. Alternatywą dla pisania do standardowego wyjścia błędów jest wykorzystanie biblioteki `Debug.Trace`, która pozwala na wypisanie informacji o przepływie programu w czasie wykonania. Implementacja pisania do standardowego wyjścia błędów jest zależna od systemu operacyjnego, więc może nie działać na niektórych platformach.

## Zobacz też:
1. [Dokumentacja funkcji `hPutStrLn` w Haskellu](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html#v:hPutStrLn)
2. [Biblioteka `Debug.Trace` w Haskellu](https://hackage.haskell.org/package/base-4.14.0.0/docs/Debug-Trace.html)