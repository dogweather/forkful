---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:03.165308-07:00
description: "Kapitalizacja ci\u0105gu znak\xF3w polega na zmodyfikowaniu ci\u0105\
  gu tak, aby jego pierwszy znak by\u0142 wielk\u0105 liter\u0105, podczas gdy reszta\
  \ ci\u0105gu pozostaje niezmieniona.\u2026"
lastmod: '2024-03-13T22:44:34.976730-06:00'
model: gpt-4-0125-preview
summary: "Kapitalizacja ci\u0105gu znak\xF3w polega na zmodyfikowaniu ci\u0105gu tak,\
  \ aby jego pierwszy znak by\u0142 wielk\u0105 liter\u0105, podczas gdy reszta ci\u0105\
  gu pozostaje niezmieniona."
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
