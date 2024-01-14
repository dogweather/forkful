---
title:                "Clojure: Generering av tilfeldige tall"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor

Generering av tilfeldige tall er en viktig del av mange programmeringsoppgaver. Det kan være nyttig for å lage tilfeldig spillinnhold, teste algoritmer eller rett og slett for å legge til variasjon i et program.

# Slik gjør du det

Det finnes flere måter å generere tilfeldige tall på i Clojure. En enkel måte er å bruke funksjonen "rand", som returnerer et tilfeldig desimaltall mellom 0 og 1.

```Clojure
(rand)
;; Output: 0.4251775647121888
```

Hvis du vil ha et heltall, kan du bruke funksjonen "rand-int". Her kan du også spesifisere et maksimalt tall som skal genereres.

```Clojure
(rand-int 10)
;; Output: 7
```

Ønsker du et tilfeldig tall innenfor et spesifikt område, kan du bruke "rand-nth" og gi en liste som parameter.

```Clojure
(rand-nth [1 2 3 4 5])
;; Output: 3
```

# Dykk dypere

Clojure bruker en pseudorandom-nummergenerator basert på en lineær kongrutermetode. Dette betyr at tallene som genereres ikke er helt tilfeldige, men er basert på en matematisk formel som gir en følge av tall som ser ut til å være tilfeldige.

Hvis du vil ha større grad av tilfeldighet i tallgenereringen, kan du bruke "seed-random" funksjonen til å sette en fast startverdi for generatoren. Dette kan være nyttig for testing og debugging.

# Se også

- [Clojure dokumentasjon for tilfeldige tall](https://clojuredocs.org/clojure.core/rand)
- [En fullstendig guide til Clojure programmetds](https://www.braveclojure.com/foreword/)
- [En liste med andre Clojure ressurser på norsk](https://github.com/kogakure/clojure-resources-norwegian)