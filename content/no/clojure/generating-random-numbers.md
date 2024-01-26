---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:02.213898-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generering av tilfeldige tall er prosessen å lage nummer som ikke kan forutsies. Programmerere trenger dette for spill, simuleringer og sikkerhet.

## How to:
For å generere tilfeldige tall i Clojure bruker vi `rand`, `rand-int` eller `rand-nth`.

```Clojure
; En tilfeldig flyttall mellom 0.0 (inklusiv) og 1.0 (eksklusiv)
(rand)

; En tilfeldig heltall mindre enn 10
(rand-int 10)

; En tilfeldig verdi fra en samling
(rand-nth [1 2 3 4 5])
```

Kjøre eksemplene gir nye tall hver gang, som dette:

```Clojure
(rand) ; => 0.7090722079867595
(rand-int 10) ; => 7
(rand-nth [1 2 3 4 5]) ; => 3
```

## Deep Dive
Randomisering har vært en del av programmering siden starten. I Clojure styres randomisering av Java's `java.util.Random` klasse bak kulissene. Alternativer til innebygde funksjoner inkluderer å bruke `java.security.SecureRandom` for kryptografisk sikker randomisering eller eksterne biblioteker for mer komplekse behov.

Implementasjonsdetaljer om `rand` og vennene er viktig: de er ikke egnet for sikkerhetskritiske formål. Denne standard randomiseringen kan forutsies hvis angriper vet eller kan gjette tilstanden. For sikkerhet, gå mot `SecureRandom`.

## See Also
- Clojure's random number generation functions: https://clojuredocs.org/clojure.core/rand
- Understanding Java's Random class: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- Crypto-strong randomness in Java: https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html
- More on random number generators (RNGs) by Clojure's creator, Rich Hickey: https://www.youtube.com/watch?v=oytL881p-nQ
