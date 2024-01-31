---
title:                "Avrunding av tall"
date:                  2024-01-26T03:43:27.380057-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"

category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Avrunding av tall handler om å justere et tall til nærmeste hele, eller til en bestemt desimalpresisjon. Vi avrunder tall for å forenkle dem for menneskelig lesbarhet, redusere beregningsbelastning, eller for å møte spesifikke numeriske krav.

## Hvordan:
I Clojure bruker vi primært `Math/round`, `Math/floor` og `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

For spesifikke desimalplasser, multipliserer vi, runder av og deler:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Dypdykk
Før fancy programmeringsspråk var avrunding en manuell prosess, tenk kuleramme eller papir. I programmering er det avgjørende for tallrepresentasjon på grunn av begrensninger med flyttalls presisjon.

Alternativer for avrunding inkluderer bruk av `BigDecimal`-klassen for presisjonskontroll eller biblioteker som `clojure.math.numeric-tower` for avanserte mattefunksjoner. Clojures `Math/round` avhenger av Javas `Math.round`, `Math/floor` og `Math/ceil`-funksjoner, noe som betyr at den arver de samme flyte- og dobbeltøyansene.

Når det gjelder implementering, husk at når du runder av i Clojure, bruker den automatisk dobbel presisjon når den håndterer desimaler. Vær forsiktig med avrundingsfeil!

## Se også
- Clojure Math API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java Math API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Forstå flyttalls presisjon: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
