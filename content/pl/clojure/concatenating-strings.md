---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Łączenie ciągów, znane jako konkatenacja, polega na łączeniu dwóch lub więcej ciągów w jeden. Programiści robią to, aby manipulować danymi tekstowymi, tworzyć nowe ciągi i zwiększyć czytelność kodu.

## Jak to zrobić:
Do łączenia ciągów w Clojure używamy funkcji `str`.

Podajemy ciągi jako argumenty do `str`:

```Clojure 
(str "Clojure" " " "jest" " " "super!")
```

Jako wynik otrzymamy:

```Clojure
"Clojure jest super!"
```

Możemy też użyć `str` do łączenia ciągów z innymi danymi, np. z liczbami:

```Clojure
(str "Mam " 5 " kotow.")
```

Tutaj otrzymamy:

```Clojure
"Mam 5 kotow."
```

## Głębsze Zanurzenie
Historia łączenia ciągów w Clojure jest związana z jej funkcją `str`, która pochodzi z języka Lisp, na którym Clojure jest oparte. Ta funkcja jest bardziej zwięzła i elastyczna niż metody stosowane w niektórych innych językach, takich jak metoda `.concat(String)` w Javie.

Alternatywą dla `str` jest funkcja `join` z biblioteki `clojure.string`. `join` łączy ze sobą ciągi z listy, umieszczając między nimi separator.

```Clojure
(clojure.string/join " " ["Clojure" "jest" "super!"])
```

Ten kod wygeneruje ten sam rezultat co nasz pierwszy przykład, tzn. "Clojure jest super!".

Szczegółami implementacji, które warto znać, jest to, że `str` łączy ciągi szybko i efektywnie, zwracając nowy ciąg. Nadmiarowe użycie `str` może jednak prowadzić do dużego zużycia pamięci, jeśli tworzy się wiele dużych ciągów.

## Zobacz Także
Chcesz dowiedzieć się więcej? Sprawdź te zasoby:

1. Oficjalna dokumentacja Clojure: https://clojure.org/guides/learn/functions
2. Dokumentacja funkcji str: https://clojuredocs.org/clojure.core/str
3. Dokumentacja biblioteki clojure.string: https://clojuredocs.org/clojure.string