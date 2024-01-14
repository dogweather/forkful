---
title:                "Clojure: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

### Dlaczego

Dlaczego ktokolwiek chciałby zajmować się wyszukiwaniem i zamianą tekstu? Gdy piszemy kod, często musimy zmienić nazwy zmiennych lub poprawić błędy w tekście. W takich sytuacjach, narzędzia do wyszukiwania i zamiany tekstu mogą nam znacznie ułatwić pracę.

### Jak to zrobić

Kod Clojure jest bardzo wygodnym i efektywnym sposobem na wyszukiwanie i zamianę tekstu. Wystarczy użyć funkcji `replace` i podać jej dwa argumenty - wzorzec wyszukiwania oraz ciąg, który chcemy wstawić. Poniżej znajdują się przykłady:

```Clojure
; Podmiana tekstu 'foo' na 'bar'
(replace "foo" "bar" "Hello foo world") ; => "Hello bar world"

; Podmiana tekstu na wielokrotność
(replace "e" "ee" "Hello world") ; => "Heello world"
```

Możemy również wykorzystać funkcję `replace-first`, aby zamienić tylko pierwsze wystąpienie wzorca:

```Clojure
; Zamiana pierwszego wystąpienia tekstu 'foo' na 'bar'
(replace-first "foo" "bar" "Hello foo foo foo") ; => "Hello bar foo foo"
```

W przypadku, gdy chcemy wyszukać i zamienić dany tekst tylko w wybranych miejscach, możemy użyć funkcji `replace-in`:

```Clojure
; Zamiana tekstu tylko w wybranym fragmencie 'foo'
(replace-in "foo" "bar" "Hello foo world" 6 9) ; => "Hello bar world"
```

### Głębsza analiza

Clojure oferuje także bardziej zaawansowane funkcje do wyszukiwania i zamiany tekstu, takie jak `replace-predicate`, `replace-kv` czy `replace-skip-rest`. Więcej informacji na temat tych funkcji znajdziesz w [dokumentacji Clojure](https://clojure.github.io/clojure/clojure.string-api.html).

### Zobacz także

- [Pełna dokumentacja funkcji replace](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/replace)
- [Przykłady wykorzystania funkcji replace](https://practicalli.github.io/clojure/core-strings/replace)
- [Dokumentacja Clojure](https://clojure.org/documentation) (dostępna także w języku polskim)