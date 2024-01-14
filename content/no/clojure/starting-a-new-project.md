---
title:    "Clojure: Å starte et nytt prosjekt"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang vurdert å starte et nytt programmeringsprosjekt, men vært usikker på om det er verdt tiden og innsatsen? Det er mange grunner til å ta steget og starte et nytt Clojure-prosjekt. For det første er Clojure et kraftig, funksjonelt programmeringsspråk som er enkelt å lese og skrive. I tillegg tilbyr Clojure et rikt økosystem av biblioteker og verktøy som gjør det enkelt å bygge komplekse og robuste applikasjoner. Uansett om du er nybegynner eller en erfaren utvikler, er Clojure et flott valg for å starte et nytt prosjekt.

## Hvordan gjøre det

Nå som du er overbevist om å starte et nytt Clojure-prosjekt, la oss se på hvordan du kan komme i gang. Først må du sørge for at Clojure er installert på datamaskinen din. Dette kan du enkelt gjøre ved å følge instruksjonene på Clojure sin offisielle nettside. Deretter kan du lage et nytt prosjekt og inkludere de nødvendige avhengighetene ved å bruke Leiningen, et byggeverktøy for Clojure-prosjekter.

```Clojure
lein new prosjektnavn
```

Nå kan du begynne å skrive kode ved hjelp av en tekstredigerer eller et utviklingsmiljø som f.eks. IntelliJ IDEA. La oss si at du vil skrive en funksjon som legger sammen to tall og skriver ut resultatet:

```Clojure
(defn legg-sammen [tall1 tall2]
  (println (+ tall1 tall2)))
```

Nå kan du kalle funksjonen ved å bruke følgende kode:

```Clojure
(legg-sammen 3 5)
```

Kjører dette vil gi følgende output:

```
8
```

Etter å ha skrevet koden, kan du teste den ved hjelp av enhetstester og sørge for at den fungerer som forventet.

## Dykk ned i det

For å få mest mulig ut av ditt nye Clojure-prosjekt, anbefales det å lære mer om språket og økosystemet rundt det. Clojure har et aktivt samfunn og det finnes mange ressurser, både på engelsk og norsk, som kan hjelpe deg å lære mer. Du kan også ta en titt på Clojure sin dokumentasjon og utforske ulike biblioteker som kan være nyttige for ditt prosjekt.

## Se også

- [Clojure.org](https://clojure.org/)
- [Clojurescript.org](https://clojurescript.org/)
- [Leiningen.org](https://leiningen.org/)
- [Clojure for Nybegynnere (på norsk)](http://clojurenorge.com/)
- [Clojure Programutvikling - Utforsk det kraftige Clojure-økosystemet (på norsk)](https://leanpub.com/cloju)