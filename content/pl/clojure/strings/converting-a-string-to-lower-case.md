---
title:                "Konwersja ciągu znaków na małe litery"
aliases:
- pl/clojure/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:17.463377-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

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
