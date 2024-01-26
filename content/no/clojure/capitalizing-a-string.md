---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Kapitalisering av en streng betyr å gjøre første bokstav i hvert ord til en stor bokstav. Programmerere kapitaliserer strenger for å følge språkregler, forbedre lesbarhet eller formatere tekstdata etter standarder.

## How to (Hvordan gjøre det)
I Clojure, for å kapitalisere hver ord i en streng, bruker vi ikke en innebygd funksjon, men du kan kombinere flere funksjoner for å oppnå dette. Her er et eksempel:

```Clojure
(defn capitalize-string [s]
  (clojure.string/join " " (map clojure.string/capitalize (clojure.string/split s #"\s+"))))
  
(capitalize-string "hei verden, dette er clojure!")
```
Output:
```
"Hei Verden, Dette Er Clojure!"
```

## Deep Dive (Dypdykk)
Clojure har ikke en innebygd funksjon for å kapitalisere alle ord i en streng direkte, til forskjell fra noen andre språk. Dette skyldes Clojures fokus på funksjonell programmering og komponerbare funksjoner. 

Alternativet vist over er å splitte strengen ved whitespace med `clojure.string/split`, kapitalisere hvert ord med `clojure.string/capitalize`, og så sette sammen ordene igjen med `clojure.string/join`. Dette er en komposisjon av funksjoner.

Implementasjonsdetaljer for `clojure.string/capitalize` inkluderer at den bare gjør første bokstav stor, om bokstaven er lav. Resten av ordet påvirkes ikke. Om du vil ha streng konvertering hvor kun første bokstav er stor og resten små, må du selv definere en ny funksjon eller komponere eksisterende på en annen måte.

Siden Clojure opererer på en Java Virtual Machine (JVM), kan du som alternativ også bruke Java sine strengmetoder direkte i Clojure.

## See Also (Se Også)
Kode og dokumentasjon:
- [Clojure Strings](https://clojuredocs.org/clojure.string)
- [ClojureDocs](https://clojuredocs.org)

Relaterte emner:
- [Functional Programming](https://clojure.org/about/functional_programming)
- [Clojure - Java Interop](https://clojure.org/reference/java_interop)
