---
title:    "Clojure: Usuwanie znaków pasujących do wzorca"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania programów w Clojure, musimy przetwarzać tekst i usuwać z niego określone znaki, które nie są nam potrzebne. Może to wynikać z różnych powodów, na przykład usuwania białych znaków z tekstu, usunięcia znaków specjalnych lub filtrowania niechcianych znaków. Dlatego w tym artykule przedstawimy jak usunąć znaki pasujące do wzoru w języku Clojure.

## Jak to zrobić

W języku Clojure możemy wykorzystać funkcję `clojure.string/replace` do usuwania znaków pasujących do wzoru. Poniżej przedstawimy przykładowe kody i wyniki dla różnych przypadków użycia:

```Clojure
(clojure.string/replace "Hello World!" #"\s" "")
```
Wynik: "HelloWorld!" - usuwa wszelkie białe znaki z tekstu.

```Clojure
 (clojure.string/replace "Hello_123_" #"\W" "")
```
Wynik: "Hello123" - usuwa znaki specjalne z tekstu.

```Clojure
(clojure.string/replace "a1b2c3d" #"[^a-zA-Z]" "")
```
Wynik: "abcd" - usuwa wszystkie znaki z wyjątkiem liter alfabetu.

## Głębsza analiza

Funkcja `clojure.string/replace` przyjmuje trzy argumenty: tekst, wzorzec i tekst zastępczy. Wzorzec może być zwykłym wyrażeniem regularnym lub ciągiem znaków. Jeśli wzorzec jest ciągiem znaków, funkcja zastępuje wszystkie wystąpienia tego ciągu w tekście. Jeśli wzorzec jest wyrażeniem regularnym, funkcja usuwa wszystkie znaki pasujące do tego wyrażenia.

## Zobacz również

Oto kilka linków, gdzie można dowiedzieć się więcej o usuwaniu znaków pasujących do wzoru w języku Clojure:

- [Dokumentacja Clojure: funkcja replace](https://clojuredocs.org/clojure.string/replace)
- [Podstawy wyrażeń regularnych w Clojure](https://medium.com/@joc_harris/regular-expressions-in-clojure-f38c57f7c716)
- [Przewodnik po języku Clojure](https://clojure.org/guides/getting_started)