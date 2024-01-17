---
title:                "Znalezienie długości ciągu znaków"
html_title:           "Clojure: Znalezienie długości ciągu znaków"
simple_title:         "Znalezienie długości ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyszukiwanie długości ciągu znaków to nic innego jak określenie, ile znaków znajduje się w danym ciągu. Programiści często potrzebują tej informacji w swoim kodzie, aby móc dynamicznie manipulować tekstem i dokonywać obliczeń.

## Jak to zrobić:

"Clojure ..." bloki kodu z przykładowymi rozwiązaniami i wynikami:

``Clojure

;; Sprawdzenie długości ciągu przy użyciu funkcji count()
(def str "To jest przykładowy ciąg")
(count str)
;; Wynik: 23

;; Wyszukiwanie długości ciągu przy użyciu funkcji .length
(.length "To jest kolejny ciąg")
;; Wynik: 23

```

## Deep Dive:

1) Kontekst historyczny: Funkcja "count" jest dostępna w języku Clojure od jego początków. Z kolei metoda ".length" jest częścią zestawu narzędzi języka Java, na którym opiera się Clojure.

2) Alternatywy: Oprócz funkcji "count" i metody ".length", istnieje również funkcja "clojure.string/length", która jest częścią standardowej biblioteki Clojure i działa także z ciągami znaków mającymi specjalne znaki.

3) Szczegóły implementacji: Funkcja "count" zawiera w sobie proces iteracji po elementach ciągu i zliczania ich liczby. Natomiast metoda ".length" wykorzystuje pole "length" obiektu ciągu znaków w języku Java, co jest znacznie szybsze, ale może generować nieoczekiwane wyniki w przypadku ciągów znaków zawierających specjalne znaki.

## Zobacz również:

- Dokumentacja funkcji "count": https://clojuredocs.org/clojure.core/count
- Dokumentacja metody ".length": https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length()