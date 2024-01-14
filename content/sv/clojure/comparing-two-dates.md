---
title:                "Clojure: Jämföring av två datum"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara en viktig del av många Clojure-program. Det kan hjälpa till att hitta skillnader mellan datum, avgöra vilket datum som kommer först eller hantera tidsbundna händelser. Låt oss titta på hur man kan jämföra två datum i Clojure.

## Så här gör du

Först måste vi definiera två datum som vi vill jämföra. Vi kan använda funktionen `date` för att skapa ett datum från dag, månad och år:

```Clojure
(def datum1 (date 2021 3 25))
(def datum2 (date 2021 5 15))
```

Nu kan vi använda funktionen `before?` eller `after?` för att jämföra dessa två datum. Dessa funktioner returnerar `true` om det första datumet kommer före det andra datumet och `false` annars.

```Clojure
(before? datum1 datum2) ;; returnerar true
(after? datum1 datum2) ;; returnerar false
```

Vi kan också använda funktionen `min` och `max` för att hitta det minsta eller största datumet mellan två givna datum:

```Clojure
(min datum1 datum2) ;; returnerar datum1
(max datum1 datum2) ;; returnerar datum2
```

Om vi behöver jämföra tidsstämpeln för ett datum, kan vi använda funktionen `instant` för att få en Unix-tidsstämpel och sedan använda aritmetiska operationer för att jämföra dessa tidsstämplar.

```Clojure
(def timestamp1 (instant datum1))
(def timestamp2 (instant datum2))
(> timestamp2 timestamp1) ;; returnerar true
```

## Djupdykning

Vid jämförelse av datum måste vi ta hänsyn till olika tidszoner och sommartid. Clojure ger oss möjlighet att använda biblioteket `clj-time` som erbjuder en mängd olika funktioner för att hantera tidszon och sommartid.

Det är också viktigt att notera att jämförelser av datum kan vara känsliga för tidsprecision, särskilt vid jämförelse av Unix-tidsstämplar. Det är viktigt att noga överväga vilken precision som är nödvändig för det aktuella användningsområdet.

## Se också

* Officiell dokumentation för Clojure `date` funktionen: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/date
* Dokumentation för `clj-time` biblioteket: https://github.com/clj-time/clj-time/wiki
* En djupare artikel om jämförelse av datum i Clojure: https://www.mikera.net/posts/2012-07-10-clojure-date-comparisons.html