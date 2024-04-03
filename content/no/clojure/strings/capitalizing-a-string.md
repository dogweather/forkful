---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:53.738362-07:00
description: "\xC5 sette stor bokstav i en streng inneb\xE6rer \xE5 endre strengen\
  \ slik at dens f\xF8rste tegn er en stor bokstav, mens resten av strengen forblir\
  \ uendret.\u2026"
lastmod: '2024-03-13T22:44:40.386166-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sette stor bokstav i en streng inneb\xE6rer \xE5 endre strengen slik\
  \ at dens f\xF8rste tegn er en stor bokstav, mens resten av strengen forblir uendret."
title: Sette stor bokstav i en streng
weight: 2
---

## Hvordan:
Clojure, som er et JVM-språk, lar deg bruke Java String-metoder direkte. Her er et grunnleggende eksempel på hvordan du setter stor bokstav i en streng i Clojure:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure inkluderer ikke en innebygd funksjon spesifikt for å sette stor bokstav i strenger, men som vist, kan du enkelt oppnå dette ved å kombinere `clojure.string/upper-case`, `subs` og `str` funksjoner.

For en mer kortfattet løsning og håndtering av mer komplekse strengmanipulasjoner, kan det hende du vender deg til et tredjepartsbibliotek. Et slikt populært bibliotek i Clojure-økosystemet er `clojure.string`. Imidlertid, per min siste oppdatering, tilbyr det ikke en direkte `capitalize`-funksjon utover det som er demonstrert med Clojures kjernefunksjonaliteter, så metoden vist ovenfor er din direkte tilnærming uten å trekke inn ekstra biblioteker spesifikt for stor bokstavsetting.

Husk, når du jobber med strenger i Clojure som samhandler med Java-metoder, jobber du effektivt med Java-strenger, noe som gjør at du kan utnytte hele arsenalt av Javas String-metoder direkte i Clojure-koden din om nødvendig.
