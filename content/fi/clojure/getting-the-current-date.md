---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Clojure: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Haluatko tietää mikä päivä tänään on? Haluatko lisätä päivämääräominaisuuden omassa ohjelmassasi? Clojuren avulla voit helposti saada nykyisen päivämäärän näkyviin.

## Miten
Käytä Clojuren *clj-time* -kirjastoa saadaksesi nykyisen päivämäärän käyttöön. Ensimmäiseksi, lisää *clj-time* riippuvuus projektiisi ja importtaa se koodiisi.

```Clojure
(require '[clj-time.core :as time])
```

Sitten voit käyttää *now* -funktiota saadaksesi nykyisen päivämäärän ja *today* -funktiota saadaksesi nykyisen päivän ilman kellonaikaa. Voit myös käyttää *date* funktiota muuttamaan päivämäärän haluamaasi muotoon.

```Clojure
(time/now) ; => #<DateTime 2020-10-16T00:00:00.000Z>
(time/today) ; => #<LocalDate 2020-10-16>
(time/date (time/today)) ; => #<DateTime 2020-10-16T00:00:00.000Z>
```

Voit myös saada nykyisen ajan käyttämällä *now* funktiota ilman parametreja.

```Clojure
(time/now) ; => #<DateTime 2020-10-16T00:00:00.000Z>
```

## Syventyminen
Voit lisätä päivämäärän avulla myös muita ominaisuuksia, kuten päivien ja kuukausien lisääminen tai vähentäminen. Voit käyttää *plus* ja *minus* funktioita muuttamaan päivämäärää haluamallasi tavalla.

```Clojure
(time/plus (time/now) (time/days 7)) ; lisää 7 päivää nykyiseen päivämäärään
(time/minus (time/now) (time/months 1)) ; vähentää 1 kuukauden nykyisestä päivämäärästä
```

Voit myös lisätä tietty määrä aikaa nykyiseen päivämäärään käyttämällä *plus* ja *minus* funktioita sekä *milliseconds*, *seconds*, *minutes*, *hours*, *days*, *weeks* ja *months* funktioita.

```Clojure
(time/plus (time/now) (time/seconds 30)) ; lisää 30 sekuntia nykyiseen päivämäärään
(time/minus (time/now) (time/weeks 2)) ; vähentää 2 viikkoa nykyisestä päivämäärästä
```

## Katso myös
- [clj-time - ClojureKirjasto](https://github.com/clj-time/clj-time)
- [Joda - Aikakirjasto Java-kielelle, johon clj-time perustuu](https://www.joda.org/joda-time/)
- [Java 8 Datetime API - Java-kirjasto päivämäärien käsittelyyn](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)