---
date: 2024-01-26 01:02:40.692405-07:00
description: "Logging er i prinsippet programvareekvivalenten til en skipslogg; det\
  \ er en m\xE5te \xE5 registrere hendelser som skjer mens en applikasjon kj\xF8rer.\
  \ Utviklere\u2026"
lastmod: '2024-03-11T00:14:13.931818-06:00'
model: gpt-4-1106-preview
summary: "Logging er i prinsippet programvareekvivalenten til en skipslogg; det er\
  \ en m\xE5te \xE5 registrere hendelser som skjer mens en applikasjon kj\xF8rer.\
  \ Utviklere\u2026"
title: "Loggf\xF8ring"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Logging er i prinsippet programvareekvivalenten til en skipslogg; det er en måte å registrere hendelser som skjer mens en applikasjon kjører. Utviklere gjør det for å holde oversikt over disse hendelsene for feilsøking, revisjonsspor, eller for å få innsikt i atferden til et system i produksjon.

## Hvordan:
Clojure støtter seg på Javas loggingsfasiliteter, men du kan utnytte dem på en mer idiomatisk Clojure-måte. La oss se på hvordan du kan bruke `clojure.tools.logging`, som gir en enkel abstraksjon over flere loggingrammeverk:

Først, legg til en avhengighet for `clojure.tools.logging` og en logg-implementering som `log4j` i din `project.clj`:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Nå, la oss logge noen meldinger:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Starter intens beregning...")
  (Thread/sleep 3000) ; Simulerer en lang beregning
  (log/info "Beregning fullført. Svaret er 42.")
  42)

(compute-answer-to-everything)
```
Utdata vil ikke vise `DEBUG` meldinger som standard, ettersom loggnivåer typisk er satt til `INFO`:

```
INFO  [ditt-navnerom] - Beregning fullført. Svaret er 42.
```

Du kan konfigurere loggnivåer og appender i en `log4j.properties`-fil for å få mer detaljerte utdata etter behov.

## Dypdykk
Clojures `clojure.tools.logging` har eksistert en stund og fungerer som en bro mellom Clojure-kode og Java-loggingsverdenen. Historisk sett har Java gjennomgått flere iterasjoner og biblioteker for logging slik som Javas innebygde logging API, `log4j`, `slf4j`, og `logback`.

I Clojure, mens du direkte kan bruke Javas loggingrammeverk, oppdager `clojure.tools.logging` og delegerer til hvilket som helst loggingrammeverk det finner i din klassebane, noe som sparer deg for å være tett koblet til en spesifikk implementering. Dette kan bidra til å holde din Clojure-kode mer portabel og modulær.

Alternativer til `clojure.tools.logging` innenfor Clojure-økosystemet inkluderer biblioteker som `timbre`, som er et rent Clojure-loggingsbibliotek med funksjoner som loggrotasjon, filtrering, og asynkron logging rett ut av boksen.

Implementeringsdetaljer er avgjørende når det kommer til logging i et flertrådet miljø som Clojure. Her gir immutabilitet og sideeffektstyring tydelige fordeler. Logging, som en sideeffekt, bør håndteres med forsiktighet for å unngå ytelsesflaskehalser og sikre trådsikkerhet, noe de fleste Java-loggingrammeverk allerede tar hånd om.

Til slutt, vurder strukturert logging, hvor logger er skrevet som strukturerte data (som JSON). Dette kan være ekstremt nyttig for senere analyse og behandling, spesielt når det gjelder storskala distribuerte systemer.

## Se også
Hvis du er ivrig etter mer, vurder å sjekke ut disse ressursene:

- Clojure Tools Logging dokumentasjon: https://github.com/clojure/tools.logging
- Timbre, et Clojure-loggingsbibliotek: https://github.com/ptaoussanis/timbre
- Konfigurering av Log4J i Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Logback Manual for avanserte oppsett: http://logback.qos.ch/manual/
- En guide til strukturert logging i Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
