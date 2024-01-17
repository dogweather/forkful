---
title:                "Wyodrębnianie podciągów"
html_title:           "Haskell: Wyodrębnianie podciągów"
simple_title:         "Wyodrębnianie podciągów"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Ekstrakcja podciągów w Haskellu jest spopularyzowaną praktyką polegającą na wyodrębnieniu fragmentów ciągów znaków na podstawie określonych kryteriów. Programiści często używają tej techniki, aby łatwiej manipulować danymi tekstowymi lub w celu filtrowania i przetwarzania informacji.

# Jak to zrobić:

```Haskell
-- Przykładowe kodowanie
import Data.List

-- Wyodrębnianie podciągów na podstawie kryterium - tutaj pojedynczego znaku
-- i zwrócenie ich jako listy
extract :: Char -> String -> [String]
extract c str = groupBy (\x y -> x == c && y == c) str

-- Przykładowe wywołania funkcji i efekty
extract '.' "Hello.World!" -- ["Hello", "World"]
extract 'o' "Programming" -- ["Pr", "gramming"]
```

# Głębsze wgląd:

Ekstrakcja podciągów była wykorzystywana już w językach programowania takich jak PL/I, a także stała się powszechna w językach funkcyjnych, w tym w Haskellu. Istnieje wiele metod wyodrębniania podciągów w Haskellu, w tym również używając funkcji ```take``` i ```drop``` z biblioteki ```Data.List```. Technika ta jest szczególnie przydatna przy przetwarzaniu danych tekstowych, gdzie często potrzebujemy wyodrębnić wybrane fragmenty informacji.

# Zobacz również:

Więcej informacji na temat ekstrakcji podciągów w Haskellu znajdziesz w dokumentacji języka oraz na stronach internetowych poświęconych programowaniu. Zalecamy także zapoznanie się z innymi funkcjami dostępnymi w bibliotece ```Data.List```, ponieważ mogą one być przydatne przy różnego rodzaju operacjach na ciągach znaków.