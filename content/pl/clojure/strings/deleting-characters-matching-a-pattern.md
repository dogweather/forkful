---
date: 2024-01-20 17:41:49.365843-07:00
description: "How to: (println (delete-matching-chars \"[0-9#]\" text))) ;; Wynik:\
  \ \"Cze\u015B\u0107! To jest test.\"."
lastmod: '2024-04-05T22:50:49.286310-06:00'
model: gpt-4-1106-preview
summary: (println (delete-matching-chars "[0-9#]" text))) ;; Wynik.
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## How to:
**Jak to zrobić:**

```Clojure
(defn delete-matching-chars [pattern text]
  (clojure.string/replace text (re-pattern pattern) ""))

;; Przykład użycia:
(let [text "Cześć!## To jest 123 test."]
  (println (delete-matching-chars "[0-9#]" text)))
;; Wynik: "Cześć! To jest  test."
```

## Deep Dive
**Wgłębienie:**

W Clojure, tak jak w innych językach programowania, teksty możemy przetwarzać wykorzystując wyrażenia regularne (regex). Wyrażenia te istnieją od wczesnych lat '50, ale Clojure wykorzystuje Java Regex API, które jest młodsze. Usunięcie znaków pasujących do wzorca jest standardową operacją w przetwarzaniu tekstów, zwłaszcza w analizie danych i skryptach. Alternatywy to między innymi operacje filtrowania sekwencji lub użycie bibliotek zewnętrznych dla specyficznych przypadków. Warte odnotowania jest, że `clojure.string/replace` potrafi przyjąć re-pattern jako argument, co ułatwia pracę z regexami w Clojure.

## See Also
**Zobacz również:**

- ClojureDocs, gdzie znajdziesz więcej na temat [`clojure.string/replace`](https://clojuredocs.org/clojure.string/replace)
- [Java Regex API](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html) - poznaj mechanizmy używane przez Clojure
- [Clojure from the ground up: regular expressions](https://aphyr.com/posts/305-clojure-from-the-ground-up-regular-expressions) - artykuł wprowadzający w wyrażenia regularne w Clojure
