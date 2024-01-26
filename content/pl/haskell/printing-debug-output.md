---
title:                "Drukowanie komunikatów debugowania"
date:                  2024-01-20T17:52:50.182752-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wypisywanie informacji diagnostycznych (debug output) to sposób na śledzenie tego, co się dzieje w programie podczas jego działania. Programiści używają tego do łapania błędów i lepszego zrozumienia przepływu programu.

## Jak to zrobić:
W Haskellu możemy używać funkcji `print` czy `putStrLn` do wypisywania danych na standardowe wyjście. Będzie wyglądało to tak:

```Haskell
main :: IO ()
main = do
  let liczba = 42
  putStrLn "Wyświetlam zawartość zmiennej liczba:"
  print liczba
```

Wyjście z tego programu:
```
Wyświetlam zawartość zmiennej liczba:
42
```

Możemy też użyć `trace` z modułu `Debug.Trace` do wypisywania informacji diagnostycznych bez zmiany typu naszej funkcji:

```Haskell
import Debug.Trace (trace)

main :: IO ()
main = print (myFunction 10)

myFunction :: Integer -> Integer
myFunction x = trace ("myFunction was called with " ++ show x) (x * 2)
```

Wyjście z debugowania będzie wyglądało tak:
```
myFunction was called with 10
20
```

## Deep Dive
W Haskellu, debugowanie może być trudniejsze przez jego leniwe wyliczenia (lazy evaluation). `print` i `putStrLn` są dobre do prostej diagnostyki. `Debug.Trace` jest używane do bardziej zaawansowanego debugowania, ale uważaj: może zaburzać leniwe wyliczenia i prowadzić do nieoczekiwanego zachowania programu.

Historia: W innych językach, jak C czy Java, funkcje wypisujące do konsoli (np. `printf` czy `System.out.println`) od zawsze były standardem. W Haskellu pojawiły się one później, wraz z rozwojem IO Monady i modułu `Debug.Trace`.

Alternatywy: dla skomplikowanych aplikacji można użyć narzędzi do profilowania (jak `ThreadScope` czy `ghc-prof`) lub zintegrowane środowiska deweloperskie (IDE) z debuggerami.

## See Also
- Haskell Documentation for `Debug.Trace`: https://hackage.haskell.org/package/base-4.16.1.0/docs/Debug-Trace.html
- ThreadScope, narzędzie do profilowania wielowątkowych aplikacji Haskell: https://wiki.haskell.org/ThreadScope
- GHC User's Guide, sekcja o profilowaniu: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/profiling.html
