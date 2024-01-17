---
title:                "Wydrukowanie danych debugowania"
html_title:           "Haskell: Wydrukowanie danych debugowania"
simple_title:         "Wydrukowanie danych debugowania"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Co i Dlaczego?

Wydrukowanie wyjścia debugowania jest procesem wykorzystywanym przez programistów do wyświetlania informacji o działaniu programu w celu zrozumienia jego stanu. Jest to często stosowane podczas procesu debugowania, aby znaleźć i naprawić błędy w kodzie.

# Jak?

Funkcją wykorzystywaną do drukowania wyjścia debugowania w Haskell jest `Debug.Trace.trace`, która przyjmuje dwa argumenty: tekst do wyświetlenia i wartość do debugowania. Poniżej znajdują się przykładowe kody i ich wyjścia, aby zobaczyć jak to działa.

```Haskell
import Debug.Trace

main = do
  let x = 10

  trace "Wartość x:" x
```

Wynik:

```Haskell
Wartość x: 10
```

## Deep Dive

Istnieje wiele alternatywnych sposobów na wydrukowanie wyjścia debugowania w Haskell, takich jak użycie biblioteki `Debug.Show`, która pozwala na drukowanie informacji o własnych typach danych. Jednak popularnym wyborem jest funkcja `Debug.Trace.trace` ze względu na swoją prostotę.

Implementacja funkcji `trace` opiera się na mechanizmie wywoływania wyjątków, co może wpływać na wydajność programu. Dlatego też ważne jest, aby nie zostawiać wywołań `trace` w produkcyjnym kodzie, ponieważ może to spowolnić działanie programu.

## Zobacz też

- [Dokumentacja funkcji trace w Hoogle](https://hoogle.haskell.org/?hoogle=trace)
- [Artykuł o debuggingu w Haskell na stronie Learn You a Haskell](http://learnyouahaskell.com/debugging)