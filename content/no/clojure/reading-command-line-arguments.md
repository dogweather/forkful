---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "Clojure: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å kunne lese kommandolinjeargumenter er en måte for programmerere å få informasjon fra brukeren på når de kjører et program. Dette kan være nyttig for å tilpasse programmet eller kjøre det med forskjellige konfigurasjoner. 

## Hvordan:

For å lese kommandolinjeargumenter i Clojure, kan du bruke funksjonen *command-line-args*. Den returnerer en liste med argumentene som ble gitt ved kjøring av programmet. 

```Clojure
(def args (command-line-args))
```

Hvis du for eksempel kjører programmet med følgende kommando: 

```
lein run arg1 arg2
```
Vil *args* være lik som dette:

```Clojure
["arg1" "arg2"]
```

## Dypdykk:

Lesing av kommandolinjeargumenter har vært en del av programmeringsverdenen siden begynnelsen av 1960-tallet. I almennhet er det ansett som en standard måte å få informasjon fra brukeren på ved kjøring av et program. Alternativt kan programmerere også bruke miljøvariabler eller spørre brukeren direkte. 

I Clojure er funksjonen *command-line-args* implementert ved å bruke JVMs *System.getProperties* til å få tilgang til argumentene. Dette er en effektiv måte å lese argumenter på, men det er også alternative biblioteker som tilbyr mer avanserte funksjoner for håndtering av kommandolinjeargumenter.

## Se også:

For mer info om *command-line-args* og andre nyttige funksjoner for å arbeide med kommandolinjeargumenter i Clojure, sjekk ut ClojureDocs: 

https://clojuredocs.org/clojure.core/command-line-args