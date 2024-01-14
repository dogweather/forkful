---
title:    "Haskell: Łączenie łańcuchów znaków"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieją wiele różnych powodów, dla których programiści używają konkatencji łańcuchów w swoich projektach Haskell. Jednym z najważniejszych jest możliwość łączenia różnych fragmentów tekstu w jeden logicznie spójny tekst, co może być niezbędne w przypadku tworzenia, na przykład, generowanych dynamicznie raportów lub komunikatów użytkownikowi.

Jednak konkatencja łańcuchów może również być wykorzystywana w celach czysto estetycznych, w celu poprawienia czytelności kodu, dzięki użyciu jednego ciągu zamiast wielu oddzielnych fragmentów. W końcu, pisanie krótszego i bardziej przejrzystego kodu jest zawsze korzystne.

## Jak to zrobić

Konkatencja łańcuchów w Haskell jest wykonywana za pomocą operatora `++`. Możemy użyć go do łączenia łąńcuchów, na przykład:

```Haskell
"Pierwszy łańcuch " ++ "Drugi łańcuch"
```

Operator `++` może również być wykorzystany do łączenia łańcuchów z innymi typami danych, takimi jak Int czy Bool. Jednak należy pamiętać, że typy muszą być zgodne, w przeciwnym razie Haskell nie będzie w stanie wykonać operacji.

## Głębszy wgląd

W przypadku przekazywania do operatora `++` więcej niż dwóch argumentów, Haskell będzie wykonywał wewnątrz zagnieżdżane wykonania, co oznacza, że ​​najpierw zostaną połączone pierwsze dwa argumenty, a następnie wynik będzie połączony z kolejnym argumentem i tak dalej.

Ponadto, operator `++` można również zapisywać w ten sposób: `[Łańcuch 1, Łańcuch 2, ..., Łańcuch n] ++ [Łańcuch (n+1), Łańcuch (n+2), ..., Łańcuch m]`, co jest szczególnie przydatne, gdy chcemy połączyć wiele łańcuchów w jednym miejscu.

## Zobacz także

- [Dokumentacja Haskell o konkatencji łańcuchów](https://www.haskell.org/tutorial/strings.html#concatenation)
- [Kalkulator Haskell dla konkatencji łańcuchów](https://www.haskell.org/hoogle/?hoogle=concat)
- [Przykład użycia operatora ++](https://stackoverflow.com/questions/27078498/how-to-concatenate-two-string-in-haskell)