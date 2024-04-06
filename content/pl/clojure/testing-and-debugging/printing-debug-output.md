---
date: 2024-01-20 17:52:10.962410-07:00
description: "How to: | Jak to zrobi\u0107: Clojure u\u017Cywa funkcji `println` do\
  \ wypisania na standardowe wyj\u015Bcie. Mo\u017Cesz te\u017C u\u017Cy\u0107 `prn`\
  \ dla danych w formacie czytelnym dla\u2026"
lastmod: '2024-04-05T21:53:36.443442-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Drukowanie komunikat\xF3w debugowania"
weight: 33
---

## How to: | Jak to zrobić:
Clojure używa funkcji `println` do wypisania na standardowe wyjście. Możesz też użyć `prn` dla danych w formacie czytelnym dla Clojure, czy `printf` dla formatowania stringów.

```Clojure
;; Proste wypisywanie wiadomości
(println "Co się dzieje w programie")
;; => Co się dzieje w programie

;; Wypisanie zmiennej i jej wartości
(def x 42)
(println "Wartość x to:" x)
;; => Wartość x to: 42

;; Wypisanie danych w formacie Clojure
(prn {:a 1 :b 2 :c 3})
;; => {:a 1, :b 2, :c 3}

;; Użycie printf dla formatowania
(printf "Jest %d rodzajów ludzi: %s i %s.\n" 2 "ci, co rozumieją binarnie" "ci, co nie")
;; => Jest 2 rodzajów ludzi: ci, co rozumieją binarnie i ci, co nie.
```

## Deep Dive | W głębię tematu
W latach 90. w Lispie, jednym z przodków Clojure, wypisywanie debugowe było już praktyką. Clojure, funkcjonalny dialekt Lispa, idzie w jego ślady. Zamiast `println`, możesz używać narzędzi jak `tap>` i `add-tap` wprowadzone w Clojure 1.10, które oferują bardziej elastyczne podejście do debugowania.

Logowanie jest alternatywą do wypisywania debugowego. Zapisuje informacje do pliku, nie zaśmiecając terminala. Można używać np. biblioteki `timbre`.

W Clojure, w przeciwieństwie do niektórych innych języków, nie ma wbudowanego systemu do zarządzania poziomami logowania. Zamiast tego, zazwyczaj wybiera się zewnętrzne biblioteki jak wspomniane `timbre`.

## See Also | Zobacz też
- Oficjalna dokumentacja `println`: https://clojuredocs.org/clojure.core/println
- `tap>` i `add-tap` wprowadzenie: https://clojure.org/news/2018/12/21/tap
- Biblioteka `timbre` dla logowania: https://github.com/ptaoussanis/timbre
