---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
aliases: - /pl/clojure/capitalizing-a-string.md
date:                  2024-02-03T19:05:03.165308-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja ciągu znaków polega na zmodyfikowaniu ciągu tak, aby jego pierwszy znak był wielką literą, podczas gdy reszta ciągu pozostaje niezmieniona. Programiści często wykonują kapitalizację ciągów, aby zapewnić spójność danych, szczególnie dla nazw i miejsc, lub aby przestrzegać reguł gramatycznych w interfejsach użytkownika.

## Jak to zrobić:
Clojure, jako język JVM, umożliwia bezpośrednie wykorzystanie metod ciągów znaków z Javy. Oto podstawowy przykład, jak skapitalizować ciąg znaków w Clojure:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "witaj świecie!") ; => "Witaj świecie!"
```

Clojure nie zawiera wbudowanej funkcji specjalnie do kapitalizacji ciągów znaków, ale jak pokazano, można to łatwo osiągnąć, łącząc funkcje `clojure.string/upper-case`, `subs` i `str`.

Dla bardziej zwięzłego rozwiązania i radzenia sobie z bardziej złożonymi manipulacjami ciągami znaków, możesz sięgnąć po bibliotekę osób trzecich. Jedną z takich popularnych bibliotek w ekosystemie Clojure jest `clojure.string`. Jednak, jak wynika z mojej ostatniej aktualizacji, nie oferuje ona bezpośredniej funkcji `capitalize` poza tym, co zostało zademonstrowane z funkcjami podstawowymi Clojure, więc metoda pokazana powyżej jest prostym podejściem bez angażowania dodatkowych bibliotek specjalnie dla kapitalizacji.

Pamiętaj, pracując z ciągami znaków w Clojure, które współdziałają z metodami Javy, faktycznie pracujesz z ciągami znaków Javy, co umożliwia Ci wykorzystanie całego arsenału metod ciągów znaków Javy bezpośrednio w Twoim kodzie Clojure, jeśli jest to konieczne.
