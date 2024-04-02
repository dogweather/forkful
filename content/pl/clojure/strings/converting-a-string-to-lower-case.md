---
date: 2024-01-20 17:38:17.463377-07:00
description: "Konwertowanie ci\u0105gu znak\xF3w na ma\u0142e litery oznacza zmian\u0119\
  \ wszystkich liter w tek\u015Bcie na ich ma\u0142e odpowiedniki. W programowaniu\
  \ robimy to dla jednolito\u015Bci\u2026"
lastmod: '2024-03-13T22:44:34.980966-06:00'
model: gpt-4-1106-preview
summary: "Konwertowanie ci\u0105gu znak\xF3w na ma\u0142e litery oznacza zmian\u0119\
  \ wszystkich liter w tek\u015Bcie na ich ma\u0142e odpowiedniki. W programowaniu\
  \ robimy to dla jednolito\u015Bci\u2026"
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## What & Why?
Konwertowanie ciągu znaków na małe litery oznacza zmianę wszystkich liter w tekście na ich małe odpowiedniki. W programowaniu robimy to dla jednolitości danych, łatwiejszego porównywania stringów i spełniania wymagań specyficznych interfejsów użytkownika.

## How to:
W Clojure, użyj `clojure.string/lower-case` do konwertowania stringów na małe litery. Proste jak paszczaka ciastko:

```clojure
(require '[clojure.string :as str])

;; Przykład konwersji
(str/lower-case "Jestem Programistą w Clojure, a Ty?")
;; Wynik: "jestem programistą w clojure, a ty?"
```

## Deep Dive
W Clojure, konwersja na małe litery nie różni się zbytnio od innych języków programowania. Używamy integrowanej funkcji `lower-case` z namespace `clojure.string`.

Historia: Koncepcja ta pochodzi z czasów, kiedy standardy kodowania i systemy porównań tekstowych były w fazie młodości. By uniknąć błędów spowodowanych różnicami w wielkości liter, programiści zaczęli konwertować teksty na małe litery.

Alternatywy: Możesz także rozbudować standardową funkcję by np. obsługiwać teksty z alfabetów innych niż łaciński. Lub użyć `clojure.string/lower-case` w połączeniu z innymi funkcjami, aby usunąć białe znaki czy znormalizować tekst.

Szczegóły implementacji: Funkcja `lower-case` wykorzystuje klasę `java.lang.String` i jej metodę `toLowerCase`, która z kolei korzysta z reguł `Locale` domyślnego dla JVM.

## See Also
- Oficjalna dokumentacja Clojure [clojure.string](https://clojuredocs.org/clojure.string/lower-case)
- JavaDoc dla klasy [String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
