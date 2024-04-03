---
date: 2024-01-20 17:28:28.982112-07:00
description: "How to: W Clojure do obliczania dat mo\u017Cna wykorzysta\u0107 bibliotek\u0119\
  \ `clj-time`, bazuj\u0105c\u0105 na Joda-Time. Poni\u017Cej przyk\u0142ady u\u017C\
  ycia."
lastmod: '2024-03-13T22:44:35.010392-06:00'
model: gpt-4-1106-preview
summary: "W Clojure do obliczania dat mo\u017Cna wykorzysta\u0107 bibliotek\u0119\
  \ `clj-time`, bazuj\u0105c\u0105 na Joda-Time."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## How to:
W Clojure do obliczania dat można wykorzystać bibliotekę `clj-time`, bazującą na Joda-Time. Poniżej przykłady użycia.

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])
(require '[clj-time.periodic :as p])

;; Dodaj tydzień do aktualnego czasu
(-> (t/now)
    (t/plus (t/weeks 1))
    (c/to-string))
;; => "2023-02-23T14:40:25.000Z"

;; Odejmij 30 dni od wybranej daty
(-> (t/date-time 2023 1 24)
    (t/minus (t/days 30))
    (c/to-string))
;; => "2022-12-25T00:00:00.000Z"
```

## Deep Dive:
Clojure używa Javy wewnętrznie, więc `clj-time` jest nakładką na Joda-Time, znany i wytrzymały silnik obsługi czasu w Javie. Alternatywy to `java.time` (biblioteka czasu w Java 8+) lub wewnętrzny moduł Clojure `java.util.Calendar`. W obliczeniach dat ważne jest uwzględnienie stref czasowych i przestępnych sekund. Rozwiązania typu Joda-Time czy `java.time` radzą sobie z tym za nas.

## See Also:
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)
- [Joda-Time](https://www.joda.org/joda-time/)
- [Clojure's java-time library](https://github.com/dm3/clojure.java-time)
