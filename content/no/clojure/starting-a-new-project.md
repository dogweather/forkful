---
title:                "Clojure: Å starte et nytt prosjekt"
programming_language: "Clojure"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hvorfor

Å starte et nytt programmeringsprosjekt kan virke overveldende, men det er en utrolig givende oppgave. Fra å lære nye ferdigheter og forbedre dine eksisterende, til å skape noe unikt og innflytelsesrikt, er det mange grunner til å dykke inn i verdenen av Clojure.

# Hvordan

For å komme i gang med et nytt Clojure-prosjekt, må du åpne en terminal og skrive følgende kommandoer:

```Clojure
lein new app prosjektnavn
cd prosjektnavn
lein run
```

Dette vil opprette en ny mappe med navnet "prosjektnavn" og en "core.clj" fil inne i den. Kjør kommandoen `lein run` for å starte programmet.

For å legge til ekstra biblioteker, kan du bruke følgende kommandoer:

```Clojure
lein new library bib-navn
lein install
```

Dette vil opprette en ny mappe kalt "bib-navn" og installere den i ditt lokale Maven-repositorium. Deretter kan du legge den til som en avhengighet i ditt prosjekt ved å inkludere `(require '[bib-navn.core :refer :all])` i "core.clj" filen.

# Dypdykk

Når du starter et nytt Clojure-prosjekt, er det viktig å planlegge godt og følge noen best practices. Å identifisere og definere funksjonaliteten til prosjektet ditt, organisere koden din i moduler og implementere enhetstesting er alle viktige aspekter for å sikre et vellykket prosjekt.

I tillegg er det viktig å følge Clojure sin konvensjonelle filstruktur, hvor hovedkoden din skal ligge i "src/" -mappen og enhetstestene i "test/" -mappen. Dette sikrer en ren og organisert kodebase som er enklere å vedlikeholde.

# Se også

- [Offisiell Clojure nettside](https://clojure.org)
- [Clojure dokumentasjon](https://clojure.org/documentation)
- [Clojure style guide](https://github.com/bbatsov/clojure-style-guide)