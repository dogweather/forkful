---
title:    "Clojure: Päivämäärän hankkiminen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Getting the current date on a computer is a common task in programming. It allows us to perform actions based on the current date, such as scheduling tasks or displaying information. In Clojure, there are multiple ways to get the current date and time, and in this blog post, we will explore these options.

## Miten

```clojure
;; Import the Java library for date and time
(ns getting-current-date.core
  (:import (java.util Calendar Date)))

;; Get the current date and time
(def today (Calendar/getInstance))

;; Get the current date
(def date (.getTime today))

;; Get the current time
(def time (.get date Calendar/HOUR_OF_DAY) (.get date Calendar/MINUTE))
```

### Tuloste:

Päivämäärän haun koodin tulosteen perusmuoto näyttää tältä:

```
Fri Apr 02 15:55:46 EEST 2021
```

## Syventävä sukellus

Clojurella on myös muita tapoja hakea tietoa nykyisestä päivämäärästä, kuten käyttämällä `instant` -funktiota:

```clojure
(require '[clojure.java-time :as t])

(t/instant)
```

Lisäksi Clojure tarjoaa `clojure.java-time` -kirjaston, joka sisältää paljon hyödyllisiä työkaluja päivämäärän ja ajan käsittelyyn. Tarkempaa tietoa tästä kirjastosta löydät [ClojureDocs](https://clojuredocs.org/clojure.java-time) -sivustolta.

## Katso myös

* [Java-tietokirjaston dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
* [Clojure.java-time dokumentaatio](https://clojure.github.io/java-time/)