---
title:    "Clojure: Utskrift av feilsøkingsutdata"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive debugging utdata er en viktig del av programmering for å feilsøke problemer og forbedre koden din.

## Slik gjør du det
Noen ganger vil du se hva som skjer i koden din, og da kan du bruke ulike typer debugging utdata for å få mer informasjon. Her er et eksempel i Clojure:

```
(defn sum [a b]
  (println "Adding numbers: " a " and " b)
  (+ a b))

(sum 2 3)
```

Dette vil skrive ut "Adding numbers: 2 and 3" og returnere 5. På denne måten kan du enkelt se hva som går inn i funksjonen og hva som kommer ut av den.

## Dykk dypere
For mer avanserte debugging tilfeller, kan du bruke funksjonen ```(clojure.pprint/pprint <verdi>)``` for å skrive ut en objekts struktur. Dette er spesielt nyttig når du arbeider med komplekse datastrukturer.

En annen nyttig metode er å bruke logging-biblioteker som "log4j" eller "logback". Disse tillater deg å skrive ut til en loggfiler som du enkelt kan analysere senere.

## Se også
- [Clojure dokumentasjon: Debugging](https://clojure.org/guides/debugging)
- [ClojureDox: clojure.pprint](https://clojuredox.org/clojure.pprint/pprint)
- [Nextjournal: Debugging in Clojure](https://nextjournal.com/sdanisch/solving-the-almighty-clojure-debugging-issue)