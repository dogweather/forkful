---
title:                "Clojure: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor?

Det er ofte nødvendig å finne ut hva som skjer under kjøring, spesielt når man arbeider med komplekse kodebaser eller feilsøker problemer. Ved å skrive ut debug-utdata kan man få en bedre forståelse av hva koden gjør, og hvor potensielle feil kan ligge.

## Hvordan gjøre det

For å skrive ut debug-utdata, kan man bruke funksjonene `prn` eller `println` i Clojure. Disse funksjonene tar inn en eller flere verdier og skriver dem ut til konsollen. La oss se på et eksempel:

```Clojure
(def num 42)
(prn "The meaning of life is:" num)
```

Dette vil skrive ut "The meaning of life is: 42" til konsollen. Man kan også bruke `println` på samme måte, men denne funksjonen legger til et ekstra nytt linjeskift på slutten.

Det er også mulig å skrive ut mer kompleks datastrukturer ved hjelp av funksjonen `pprint`. Denne funksjonen sørger for at utdataen blir formatert på en mer lesbar måte. La oss se på et eksempel:

```Clojure
(def person {:name "John Doe" :age 30 :country "Norway"})
(pprint person)
```

Dette vil skrive ut person-objektet på en fin måte til konsollen:

```
{:name "John Doe",
 :age 30,
 :country "Norway"}
```

## Dypdykk

Når man skal skrive ut debug-utdata, er det viktig å være bevisst på hva man skriver ut. Selv om det kan være fristende å skrive ut alle variablene i et program, kan dette føre til unødvendig mye utdata. Det er bedre å være selektiv og kun skrive ut de verdiene man faktisk trenger for å forstå koden.

Det er også viktig å huske at debug-utdata ikke bør være en permanent del av koden. Når man har løst problemet og forstått hva som skjer, bør man fjerne debug-utdataen for å unngå å forurense koden.

## Se også

- [Clojure debug guide](https://clojure.org/guides/debugging)
- [Debugging in Clojure - Learn Clojure](https://www.learn-clojure.com/clojure/debugging-in-clojure/)
- [Instrumenting code for debugging in Clojure](https://alexdvance.com/blog/instrumenting-code-for-debugging-in-clojure/)