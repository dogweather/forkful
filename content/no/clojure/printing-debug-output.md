---
title:    "Clojure: Utskrift av feilsøkingsutdata"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Mens du arbeider med Clojure kode, kan du komme over situasjoner der noe ikke fungerer som forventet. Dette er ofte forvirrende og frustrerende, spesielt når det er vanskelig å finne feilen i koden. Ved å legge til debug-utskrift i koden din, kan du enkelt følge med på hva som skjer og finne feilen raskere. 

## Hvordan

For å legge til debug-utskrift i Clojure-koden din, kan du bruke funksjonen "println". Denne funksjonen tar en eller flere argumenter og skriver dem ut i konsollen. Her er et eksempel på hvordan du kan bruke den:

```Clojure
(defn add [x y]
  (println "Legger sammen tallene:" x y)
  (+ x y))
  
(add 5 3)
```

Output:

```
Legger sammen tallene: 5 3
8
```

Som du kan se, vil debug-utskriften bli vist i konsollen, noe som gjør det enkelt å følge med på hva som skjer i koden din. Du kan også legge til variabelnavnet for å få mer spesifikk informasjon om verdiene som blir brukt. 

## Dypdykk

Det finnes flere måter å legge til debug-utskrift på i Clojure-koden din. En annen metode er å bruke "prn" funksjonen, som kan skrive ut datastrukturen til et objekt. Du kan også bruke "pr-str" funksjonen for å få en strengrepresentasjon av objektet. En annen nyttig funksjon for debugging er "pprint", som gir deg en mer lesevennlig utskrift av komplekse datastrukturer. 

Husk også at du kan bruke betingede uttrykk i debug-utskrifter for å kontrollere når de blir aktivert. For eksempel kan du bruke "(when condition (println "Debug-utskrift"))" for å bare få utskriften når en spesiell betingelse er oppfylt. Dette kan bidra til å redusere mengden av debug-utskrifter og gjøre det enklere å følge med på hva som skjer i koden din. 

## Se også

- [Offisiell Clojure dokumentasjon](https://clojuredocs.org/quickref)
- [Article about debugging in Clojure](https://www.braveclojure.com/debugging-clojure/)