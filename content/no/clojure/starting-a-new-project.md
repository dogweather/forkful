---
title:                "Å starte et nytt prosjekt"
html_title:           "Clojure: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når du starter et nytt prosjekt i Clojure, betyr det å sette opp alle nødvendige filer og installasjoner for å begynne å kode. Programmere gjør dette for å ha en organisert struktur for deres arbeid og for å enklere kunne samarbeide med andre utviklere.

## Hvordan:
For å begynne et nytt Clojure-prosjekt, følg disse stegene:

```Clojure
lein new app navn-på-prosjektet
cd navn-på-prosjektet
lein run
```

Output:

Velkommen til ditt nye Clojure-prosjekt!

## Dypdykk:
Det er flere alternativer for å opprette et nytt prosjekt i Clojure, som for eksempel å bruke en webbasert løsning eller en annen byggverktøy enn Leiningen. Disse kan være mer tilpassede til ditt spesifikke prosjekt og din personlige preferanse. Når du starter et nytt prosjekt i Clojure, blir det laget en "project.clj"-fil som inneholder informasjon om avhengigheter og annen konfigurasjon. Dette kan justeres og tilpasses etter dine behov.

## Se også:
[Offisiell Leiningen dokumentasjon](https://leiningen.org/) <br>
[Online Clojure prosjekt initieringsverktøy](https://clsj.build/)