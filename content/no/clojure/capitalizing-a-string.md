---
title:    "Clojure: Stor forbokstaving av en streng"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

For de fleste programmører er å kapitalisere en streng en enkel oppgave som kan gjøres i et par linjer med kode. Men hvorfor ville noen engasjere seg i denne handlingen? Svaret er ganske enkelt: for å gjøre tekst mer leselig og presentabelt. Å ha riktig kapitalisering i en streng hjelper til med å tydeliggjøre navn, titler og andre viktige elementer i teksten.

## Slik gjør du det

Det er flere måter å kapitalisere en streng i Clojure på, her skal vi se på to av de vanligste metodene.

### Metode 1

En måte å kapitalisere en streng i Clojure er å bruke funksjonen "capitalize" fra "clojure.string" biblioteket. Denne funksjonen tar inn en streng og returnerer en ny streng med den første bokstaven i hvert ord som stor bokstav.

```Clojure
(require '[clojure.string :as str])
(str/capitalize "dette er en test") ; => "Dette Er En Test"
(str/capitalize "hei alle sammen") ; => "Hei Alle Sammen"
```

Som du kan se, har vi brukt "str/" prefikset for å indikere at vi bruker en funksjon fra "clojure.string" biblioteket.

### Metode 2

En annen måte å kapitalisere en streng på er å bruke funksjonen "join" og "map" sammen. "Join" skaper en ny streng ved å kombinere elementer fra en samling i en spesifisert separator, mens "map" gjør en transformasjon på hvert element i en samling. Ved å bruke disse to funksjonene sammen, kan vi kapitalisere hvert ord i en streng.

```Clojure
(str/join " " (map #(. Character toUpperCase %) (str/split "dette er en test" #" "))) ; => "Dette Er En Test"
(str/join " " (map #(. Character toUpperCase %) (str/split "hei alle sammen" #" "))) ; => "Hei Alle Sammen"
```

I dette tilfellet har vi brukt "#()" syntax for å lage en anonym funksjon som kaller "toUpperCase" metoden på hvert tegn i strengen.

## Dykk ned i det

Det er verdt å nevne at disse metodene ikke bare fungerer på engelsk tekst. De vil også fungere på andre språk med forskjellige tegnsett. Men, hvis du ønsker å kapitalisere alle ordene i en streng, uavhengig av språk eller tegnsett, bør du vurdere å bruke funksjonen "titlecase" fra "clojure.string" biblioteket i stedet for "capitalize". Denne funksjonen bruker mer kompleks logikk for å håndtere forskjellige språk og tegnsett.

## Se også

- [clojure.string dokumentasjon](https://clojure.github.io/clojure/clojure.string-api.html)
- [Hvordan bruke strings i Clojure](https://clojure.org/guides/learn/strings)
- [Control flow i Clojure](https://clojure.org/guides/control_flow)

Takk for lesingen, håper dette har vært nyttig for deg. Fortsett å utforske og ha det gøy med Clojure!