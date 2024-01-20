---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Begynne Et Nytt Prosjekt Med Clojure

## Hva & Hvorfor?

Å starte et nytt prosjekt handler om å lage en ny kodebase fra scratch. Programmerere gjør dette for å løse unike problem eller for å bygge nye funksjoner og applikasjoner.

## Hvordan gjøre det:

Lager et nytt Clojure-prosjekt med Leiningen er rett frem. Her er en eksempel på hvordan du gjør det:

```Clojure
;; Installer Leiningen først
$ brew install leiningen

;; Bruk Leiningen for å opprette et nytt prosjekt
$ lein new awesome-app
```
Da har du laget ditt første Clojure-prosjekt! Du kan se at en prosjektkatalog med navnet "awesome-app" er opprettet.

## Deep Dive

Historisk sett ble prosjektstyring i Clojure ofte håndtert med Leiningen, men det er også andre alternativ å velge mellom. En populær en er "Boot", som gir mer fleksibilitet og kontroll. Men for nybegynnere, gir Leiningen en enklere inngangsport til Clojure-prosjekt håndtering.

Det er viktig å merke seg at hvert prosjekt du oppretter kommer med en standard struktur. Denne inkluderer essensielle biblioteker og oppsett for testing, noe som gjør det enklere for deg å komme i gang med programmeringen.

## Se Også

1. [Clojure - Getting Started](https://clojure.org/guides/getting_started)
3. [The Boot Clojure Build Tool](https://boot-clj.com/)