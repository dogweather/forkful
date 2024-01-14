---
title:    "Clojure: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Calculating a date in the future or past can be a useful skill for anyone working with time-sensitive data or dealing with date-based calculations. Whether you need to schedule events, track deadlines, or simply keep track of time, understanding how to calculate dates in the future or past can make your work easier and more efficient.

## Miten

```Clojure
; Lisätään tarvittava "clj-time" -kirjasto
(require '[clj-time.core :as time])
; Määritetään haluttu alkuperäinen päivämäärä
(def date (time/today))
; Lisätään 30 päivää alkuperäiseen päivämäärään
(time/plus date (time/days 30))
; Output: #date "2020-07-23"

; Vaihtoehtoisesti voit myös vähentää päiviä
(time/minus date (time/days 15))
; Output: #date "2020-06-08"

```

## Syvällinen sukellus

Calculating dates in the future or past involves understanding how to work with dates and time in Clojure. The "clj-time" library allows us to manipulate time objects and perform operations like addition and subtraction. We can also work with different units of time, such as years, months, weeks, and days, to accurately calculate dates in the future or past.

## Katso myös

- [Clj-time library](https://github.com/clj-time/clj-time)
- [Calculating dates and times in Clojure](https://purelyfunctional.tv/guide/calculating-dates-and-times-in-clojure/)