---
date: 2024-01-26 03:43:40.212116-07:00
description: "Zaokr\u0105glanie liczb polega na dostosowaniu liczby do najbli\u017C\
  szej liczby ca\u0142kowitej lub okre\u015Blonej precyzji dziesi\u0119tnej. Liczby\
  \ zaokr\u0105glamy w celu ich\u2026"
lastmod: '2024-03-13T22:44:34.989457-06:00'
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb polega na dostosowaniu liczby do najbli\u017Cszej\
  \ liczby ca\u0142kowitej lub okre\u015Blonej precyzji dziesi\u0119tnej."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

## Jak to zrobić:
W Clojure głównie używamy `Math/round`, `Math/floor` i `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Dla konkretnych miejsc po przecinku mnożymy, zaokrąglamy i dzielimy:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Szczegółowa analiza
Przed erą zaawansowanych języków programowania, zaokrąglanie było procesem manualnym, myśl o abakusie lub papierze. W programowaniu jest to kluczowe dla reprezentacji liczbowej ze względu na ograniczenia dokładności liczby zmiennoprzecinkowej.

Alternatywami dla zaokrąglania są użycie klasy `BigDecimal` do kontroli precyzji lub bibliotek jak `clojure.math.numeric-tower` dla zaawansowanych funkcji matematycznych. `Math/round` w Clojure opiera się na funkcjach `Math.round`, `Math/floor` i `Math/ceil` z Javy, co oznacza, że dziedziczy te same niuanse dotyczące typów float i double.

Pod kątem implementacji, pamiętaj, że przy zaokrąglaniu w Clojure automatycznie używana jest podwójna precyzja przy operacjach na liczbach dziesiętnych. Uważaj na błędy zaokrąglenia!

## Zobacz również
- API matematyczne Clojure: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- API matematyczne Java: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Zrozumienie dokładności liczby zmiennoprzecinkowej: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
