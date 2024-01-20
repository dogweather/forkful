---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til små bokstaver er prosessen med å endre store bokstaver i en tekststreng til små bokstaver. Programmerere gjør dette for å standardisere tekstdata, spesielt når de sammenligner, sorterer eller søker etter tekst.

## Hvordan gjør vi det:
Å konvertere en streng til små bokstaver i Clojure er rett fram med `clojure.string/lower-case` funksjonen. Her er et eksempel:

```Clojure
(require '[clojure.string :as str])

(str/lower-case "Clojure RULES!") 
```

Koden ovenfor vil gi følgende utskrift:

```Clojure
"clojure rules!"
```

##Dypdykk

Historisk sett var det ikke noe behov for å konvertere en streng til små bokstaver i tidlige datasystemer som behandlet strengdata som tall. Men, med økningen i databehandlingsbehov og tekstbehandling, har behovet for tekstmanipulasjonsfunksjoner som dette blitt mer tydelig.

Alternativt kan vi bruke `map` funksjonen sammen med `Character/lowerCase` for å oppnå det samme. Her er et eksempel:

```Clojure
(let [s "Clojure RULES!!"]
 (apply str (map clojure.lang.Character/lowerCase s)))
```

Når det gjelder implementasjonsdetaljer, bruker Clojure's innebygde `clojure.string/lower-case` funksjonen Java's `Character.toLowerCase` under hetta. Den tar en streng, itererer gjennom hver karakter i strengen, konverterer karakteren til små bokstaver om nødvendig, og returnerer en ny streng.

##Se Også

Mens denne implementasjonen er grei, vil du kanskje vite mer om funksjonene vi har brukt. For det kan du sjekke ut følgende:

- Clojure.string dokumentasjon [her](https://clojuredocs.org/clojure.string/lower-case)

- Clojure.lang.Character dokumentasjon [her](https://clojuredocs.org/clojure.core/Character) 

- Java's `Character.toLowerCase` dokumentasjon [her](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html#toLowerCase-char-)