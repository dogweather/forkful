---
title:                "Haskell: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto czytać argumenty wiersza poleceń? Czytanie argumentów wiersza poleceń jest niezbędnym elementem programowania w Haskellu. Wiele aplikacji wymaga wprowadzania argumentów z wiersza poleceń, więc ważne jest, abyśmy wiedzieli, jak to zrobić.

## Jak to zrobić

Często zdarza nam się mieć aplikację, która wymaga wprowadzenia pewnych parametrów z wiersza poleceń. W Haskellu możemy to zrobić za pomocą funkcji `getArgs` z modułu `System.Environment`. Poniżej znajduje się przykładowy kod, który wyświetli wszystkie argumenty wiersza poleceń wraz z ich indeksem:

```Haskell
import System.Environment

main = do
    arguments <- getArgs
    print arguments
```

Przykładowy output dla wywołania programu z argumentami `haskellBlogPost test arg1 123`:

```
["test", "arg1", "123"]
```

W ten sposób możemy łatwo odczytać argumenty i wykorzystać je w naszej aplikacji.

## Głębszy wgląd

Funkcja `getArgs` zwraca listę argumentów wiersza poleceń jako listę stringów. Warto zauważyć, że pierwszy element listy jest nazwą pliku wykonywalnego, więc musimy wziąć pod uwagę ten fakt w naszym kodzie.

W przypadku, gdy potrzebujemy przekazać do aplikacji argumenty, które zawierają spacje lub znaki specjalne, musimy je podać w cudzysłowiu.

## Zobacz także

- [Dokumentacja modułu `System.Environment`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)
- [Przykłady czytania argumentów wiersza poleceń w Haskellu](https://wiki.haskell.org/Command_line_argument_processing)