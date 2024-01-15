---
title:                "Usuwanie znaków pasujących do wzorca."
html_title:           "Clojure: Usuwanie znaków pasujących do wzorca."
simple_title:         "Usuwanie znaków pasujących do wzorca."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania kodu w Clojure zdarza się, że potrzebujemy usunąć z ciągu znaków wszystkie wystąpienia pewnego wzorca. Może to być konieczne przy przetwarzaniu danych lub wstępnej obróbce tekstu. W tym artykule dowiesz się, dlaczego i jak można usuwać znaki pasujące do pewnego wzorca w Clojure.

## Jak to zrobić

```Clojure
;; Definiowanie funkcji do usuwania znaków pasujących do wzorca
(defn delete-characters [pattern string]
  (-> string
      (clojure.string/replace pattern "")
      (clojure.string/trim)))
```
```Clojure
;; Przykładowe wywołanie funkcji
(delete-characters #"[aeiou]" "Hedvig")
;; Output: "Hdvg"
```

Powyższy kod definiuje funkcję `delete-characters`, która przyjmuje dwa argumenty: wzorzec i ciąg znaków. Wywołuje ona funkcję `clojure.string/replace`, która zastępuje wszystkie wystąpienia wzorca pustym ciągiem znaków. Następnie, przy użyciu funkcji `clojure.string/trim`, usuwane są nadmiarowe spacje z początku i końca rezultatu. W przykładzie wywołujemy funkcję z wyrażeniem regularnym `#"[aeiou]"`, które oznacza, że zostaną usunięte wszystkie samogłoski z ciągu `Hedvig`.

Przykładowy wynik wywołania funkcji to `Hdvg`, co potwierdza, że zostały usunięte wszystkie wystąpienia samogłosek. Dzięki temu prostemu kodowi, możemy łatwo usuwać znaki pasujące do dowolnego wzorca w różnych tekstach.

## Wnikliwe spojrzenie

Funkcja `clojure.string/replace` przyjmuje dwa argumenty: ciąg znaków i wzorzec. Pierwszy argument jest ciągiem znaków, w którym zostanie dokonana zamiana, a drugi jest wzorcem, który będzie znakiem lub wyrażeniem regularnym. W przypadku, gdy podamy wyrażenie regularne, może ono zawierać metaznaki, które określają ogólne wzorce znaków, np. `.` oznacza dowolny znak, `+` oznacza jeden lub więcej wystąpień, a `*` oznacza zero lub więcej wystąpień. Dzięki temu możemy precyzyjnie określić, jakie znaki chcemy usunąć.

Możemy również wykorzystać funkcję `clojure.string/replace` do zamiany znaków pasujących do wzorca na inne znaki. W tym celu, wystarczy zmienić argument z pustego ciągu na dowolny inny.

## Zobacz również

- [Funkcja `replace` z biblioteki `clojure.string`](https://clojuredocs.org/clojure.string/replace)
- [Wyrażenia regularne w Clojure](https://www.tutorialspoint.com/clojure/clojure_regular_expressions.htm)