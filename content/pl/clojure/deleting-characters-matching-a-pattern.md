---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie znaków pasujących do wzoru polega na wykryciu i usunięciu specyficznych znaków z łańcucha tekstu na podstawie określonego wzoru. Programiści używają tej techniki, aby manipulować danymi tekstowymi, np. do czyszczenia danych wejściowych, usuwania niepotrzebnych spacji, etc. 

## Jak to zrobić:
Clojure umożliwia usuwanie znaków pasujących do wzoru za pomocą funkcji `clojure.string/replace`. Wzór to najczęściej wyrażenie regularne. 

```Clojure
(require '[clojure.string :as str])

(def text "Ala ma kota, a kot ma Alę.")

(def pattern #",") ; wzór do usunięcia - przecinki 

(def clean-text (str/replace text pattern "")) ; funkcja usuwająca wzór

(println clean-text)
```

Po uruchomieniu powyższego kodu, wynik jaki otrzymamy to:

```
Ala ma kota a kot ma Alę.
```
Jak widać, wszystkie przecinki zostały usunięte z tekstu.

## Głębsze spojrzenie
Historia technik usuwania znaków pasujących do wzoru sięga początków informatyki. Wyrażenia regularne, które są podstawą tych technik, zostały wprowadzone w latach 50-tych XX wieku.

Alternatywą dla użycia `str/replace` może być napisanie własnej funkcji, która przekształca łańcuch wejściowy na sekwencję, przetwarza ją i konwertuje z powrotem na łańcuch. Ale to już bardziej skomplikowane i mniej wydajne rozwiązanie.

Implementacja `str/replace` w Clojure jest oparta o wyrażenia regularne z Javy i metody `java.lang.String.replace`.

## Zobacz również
- Dokumentacja Clojure [clojure.string/replace](https://clojuredocs.org/clojure.string/replace)
- Wyrażenia regularne w Javie: [Java Regex](https://docs.oracle.com/javase/tutorial/essential/regex/)
- O wyrażeniach regularnych na [Wikipedii](https://pl.wikipedia.org/wiki/Wyrażenie_regularne)