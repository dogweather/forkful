---
title:                "Clojure: Att hitta längden på en sträng"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande programmeringsutmaning som är användbar i många olika situationer, oavsett vilket programmeringsspråk man använder. I Clojure är det också ett bra sätt att introducera och öva med sekvenser, vilket är en viktig del av språket.

## Hur man gör

För att hitta längden på en sträng i Clojure använder vi funktionen "count". Det här är en inbyggd funktion som tar en sekvens som argument och returnerar antalet element i sekvensen. Eftersom en sträng är en samling av tecken, så kommer "count" att räkna antalet tecken i strängen.

```Clojure
(count "Hej, världen!") ; => 13
```

Vi kan också använda "count" på en vektor, en lista eller en annan typ av sekvens:

```Clojure
(count ["a" "b" "c" "d"]) ; => 4
(count (range 10)) ; => 10
```

## Fördjupning

Bortsett från att bara använda "count" för att hitta längden på en sträng, så kan vi också använda funktionen "count" inuti andra funktioner. Till exempel, om vi har en vektor av strängar och vi bara vill ha de strängar som är längre än 10 tecken, så kan vi använda "count" tillsammans med "filter" funktionen:

```Clojure
(def strängar ["Clojure" "är" "ett" "fantastiskt" "programmeringsspråk"])

(filter #(> (count %) 10) strängar) ; => ("programmeringsspråk")
```

Vi kan också använda "count" för att räkna antalet tecken i en sträng utan att behöva använda funktionen "count". Till exempel:

```Clojure
(def sträng "Ikväll ska jag äta middag med mina vänner.")

(count sträng) ; => 45
```

## Se även

- [Clojure Dokumentation: count](https://clojuredocs.org/clojure.core/count)
- [Clojure Dokumentation: filter](https://clojuredocs.org/clojure.core/filter)