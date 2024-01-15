---
title:                "Drukowanie wyjścia debugowania"
html_title:           "Haskell: Drukowanie wyjścia debugowania"
simple_title:         "Drukowanie wyjścia debugowania"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego
Debugowanie jest nieodłączną częścią tworzenia oprogramowania i często jest to jedna z najbardziej czasochłonnych czynności. Jednym z najważniejszych narzędzi w procesie debugowania jest wydruk debugowania, który pomaga programistom w identyfikacji błędów i weryfikacji poprawności działania kodu. W tym artykule dowiesz się, dlaczego wydruk debugowania jest tak ważny i jak można go wykorzystać w swoim kodzie w języku Haskell.

## Jak to zrobić
Aby wypisać wydruk debugowania w języku Haskell, wystarczy użyć funkcji `print` z modułu `Debug.Trace`. Na przykład:

```Haskell
import Debug.Trace

main = do
    let a = 5
    print a
    let b = a + 3
    print b
```

Output:
```Haskell
5
8
```

Możliwe jest również użycie funkcji `trace` z tego samego modułu, która dodaje własny tekst do wydruku. Na przykład:

```Haskell
import Debug.Trace

main = do
    let a = 5
    trace "Początek wykonywania" $ print a
    let b = a + 3
    trace "Zakończenie wykonywania" $ print b
```

Output:
```Haskell
"Początek wykonywania"
5
"Zakończenie wykonywania"
8
```

## Deep Dive
Funkcje `print` i `trace` są niezwykle przydatne podczas debugowania, ale należy pamiętać, że używanie ich w kodzie produkcyjnym może spowolnić działanie programu. Dlatego ważne jest, aby wyłączać wydruk debugowania przed uruchomieniem kodu w środowisku produkcyjnym. Można to zrobić, ustawiając zmienną środowiskową `DEBUG` na `False` lub korzystając z flagi kompilacji `-fno-print-bindings`.

Dodatkowo, funkcje `print` i `trace` mogą być również używane do wypisywania wartości zmiennych wewnątrz funkcji. W tym celu należy dodać funkcję `traceShow` przed wypisaniem wartości. Na przykład:

```Haskell
import Debug.Trace

fun x y = traceShow x $ y + x
main = print $ fun 5 3
```

Output:
```Haskell
8
```

## Zobacz też
- [Dokumentacja Debug.Trace](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)
- [Podstawy debugowania w Haskellu](https://dev.to/jacquespescatore/debugging-in-haskell-the-basics-5581)
- [Przykłady użycia debugowania w języku Haskell](https://hackernoon.com/using-debugging-in-haskell-part-1-38bc6cab2db1)