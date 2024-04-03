---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:03.165308-07:00
description: "Jak to zrobi\u0107: Clojure, jako j\u0119zyk JVM, umo\u017Cliwia bezpo\u015B\
  rednie wykorzystanie metod ci\u0105g\xF3w znak\xF3w z Javy. Oto podstawowy przyk\u0142\
  ad, jak skapitalizowa\u0107 ci\u0105g\u2026"
lastmod: '2024-03-13T22:44:34.976730-06:00'
model: gpt-4-0125-preview
summary: "Clojure, jako j\u0119zyk JVM, umo\u017Cliwia bezpo\u015Brednie wykorzystanie\
  \ metod ci\u0105g\xF3w znak\xF3w z Javy."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

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
