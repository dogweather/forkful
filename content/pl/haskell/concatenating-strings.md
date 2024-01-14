---
title:                "Haskell: Lączenie ciągów znaków"
simple_title:         "Lączenie ciągów znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto stosować łączenie stringów w języku Haskell? Jedną z najważniejszych zalet jest to, że pozwala na skuteczne budowanie tekstów w trakcie działania programu. To niezwykle przydatna funkcjonalność, szczególnie gdy mamy do czynienia z aplikacjami obsługującymi wprowadzane przez użytkownika dane.

## Jak to zrobić

Aby skorzystać z możliwości łączenia stringów w Haskellu, musimy korzystać z funkcji `++`. Przykład prostej konkatenacji dwóch stringów wygląda następująco:

```Haskell
"Hello, " ++ "World!" = "Hello, World!"
```

Warto zauważyć, że nie jest to operacja mutacyjna, a więc nie modyfikuje istniejących zmiennych, tylko tworzy nowy tekst. Możemy również łączyć większą liczbę stringów jednocześnie, na przykład:

```Haskell
"Hello " ++ "to " ++ "all " ++ "Polish " ++ "readers!" = "Hello to all Polish readers!"
```

Kolejnym ciekawym sposobem wykorzystania operatora `++` jest tworzenie list poprzez łączenie elementów za pomocą tego operatora. Przykład:

```Haskell
["Good", "morning"] ++ ["Polish", "readers!"] = ["Good", "morning", "Polish", "readers!"]
```

## Głębszy zanurzenie

Istnieje wiele różnych sposobów wykorzystania łączenia stringów w Haskellu. Możemy na przykład stosować funkcję `++` do łączenia stringów wewnątrz innych funkcji. Możemy również wykorzystywać wzorce do łączenia stringów, co pozwala na większą elastyczność i możliwość dynamicznego tworzenia tekstu. 

Ważne jest również pamiętanie o tym, że operator `++` do łączenia stringów działa również na liście znaków, więc nie musimy zamieniać tekstu na listę przed użyciem tej funkcji.

## Zobacz także

- Dokumentacja języka Haskell: https://www.haskell.org/documentation/
- Wzorce w Haskellu: https://wiki.haskell.org/Pattern_matching
- Learn You a Haskell: http://learnyouahaskell.com/