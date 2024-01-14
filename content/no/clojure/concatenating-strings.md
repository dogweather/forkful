---
title:                "Clojure: Sammenføyning av tekststrenger"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å kombinere strenger i Clojure? Det kan være nyttig når du trenger å sette sammen informasjon fra ulike kilder for å presentere for brukeren, eller hvis du trenger å formatere data på en spesifikk måte.

## Slik gjør du det

Å kombinere strenger i Clojure kan gjøres ved å bruke funksjonen "str". Her er et eksempel på hvordan dette kan gjøres:

```Clojure
(str "Hei " "verden!")
```

Dette vil produsere outputen "Hei verden!". Som du kan se, blir strengene "Hei" og "verden!" satt sammen til en ny streng. Du kan også kombinere flere strenger på en gang:

```Clojure
(str "Min favorittfarge er " "blå, " "og jeg liker å " "spise sushi.")
```

Dette vil produsere outputen "Min favorittfarge er blå, og jeg liker å spise sushi.". Du kan også kombinere strenger med variabler:

```Clojure
(def favorittmat "taco")
(str "Jeg elsker å spise " favorittmat " på fredagskvelder.")
```

Dette vil produsere outputen "Jeg elsker å spise taco på fredagskvelder.".

## Dypere dykk

I Clojure kan du også bruke funksjonen "clojure.string/join" for å kombinere en liste med strenger. Dette er nyttig hvis du har en liste med ord eller setninger som du vil sette sammen til en ny streng. Her er et eksempel på hvordan dette kan gjøres:

```Clojure
(require '[clojure.string :as str])
(def favorittlag ["Barcelona" "Real Madrid" "Liverpool"])
(str/join ", " favorittlag)
```

Dette vil produsere outputen "Barcelona, Real Madrid, Liverpool". Ved å bruke "join" kan du også legge til et skilletegn mellom hver streng. Her er et eksempel på det:

```Clojure
(require '[clojure.string :as str])
(def tall [1 2 3 4 5])
(str/join ", " tall)
```

Dette vil produsere outputen "1, 2, 3, 4, 5".

## Se også

Her er noen nyttige ressurser for å lære mer om å kombinere strenger i Clojure:

- [Clojure dokumentasjon for "str"](https://clojuredocs.org/clojure.core/str)
- [Clojure dokumentasjon for "clojure.string/join"](https://clojuredocs.org/clojure.string/join)
- [En tutorial om å jobbe med strenger i Clojure](https://www.braveclojure.com/do-things-with-strings/)