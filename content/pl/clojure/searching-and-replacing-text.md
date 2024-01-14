---
title:    "Clojure: Wyszukiwanie i zamiana tekstu"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Manipulowanie tekstem jest nieodłączną częścią programowania. Wiedza na temat wyszukiwania i zamiany tekstu jest kluczowa dla każdego programisty, który chce być efektywny w swojej pracy. W tym artykule dowiesz się, jak wykorzystać funkcje języka Clojure do efektywnego wyszukiwania i zamiany tekstu.

## Jak To Zrobić

W Clojure istnieje wiele sposobów na wyszukiwanie i zamianę tekstu. Kilka przykładowych metod pokazano poniżej:

### Wyszukiwanie i zamiana tekstu w ciągu znaków

```
Clojure (str/replace "Cześć, jak się masz?" #"masz" "ma się")
```

Wynik: "Cześć, jak się ma?"

### Wyszukiwanie i zamiana tekstu w liście

```
Clojure (map #(str/replace % #"w" "ww") ["program" "kod" "komputery"])
```

Wynik: ("programm" "kodd" "kompwwutery")

### Wyszukiwanie i zamiana tekstu w mapie

```
Clojure (apply str/replace {"a" "b" "hello" "hi"} ["a hello" "b hello"])
```

Wynik: "b hi"

## Głębsza Analiza

W przypadku bardziej złożonych zadań wyszukiwania i zamiany tekstu, funkcja `str/replace` może okazać się niewystarczająca. W takiej sytuacji warto zapoznać się z funkcjami takimi jak `re-seq`, `re-seq*` czy `re-find`, które pozwalają na bardziej precyzyjne wyszukiwanie i manipulowanie tekstu.

Inną przydatną techniką jest wykorzystanie wyrażeń regularnych (ang. regular expressions), które pozwalają na bardzo dokładne określenie wzorców tekstu do wyszukania i zamiany. Dzięki nim można wykonać zaawansowane operacje, np. zmienić wielkość liter w całym tekście lub wyodrębnić określone elementy z ciągu znaków.

## Zobacz Również

- Oficjalna dokumentacja Clojure dotycząca wyszukiwania i zamiany tekstu: https://clojure.org/api/cheatsheet#re
- Przewodnik po wyrażeniach regularnych w języku Clojure: https://clojuredocs.org/clojure.core/re-matches
- Przykłady wykorzystania funkcji `re-find` i `re-seq` w Clojure: https://clojureverse.org/t/regular-expressions-regexp-in-clojure/4241