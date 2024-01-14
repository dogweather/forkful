---
title:    "Clojure: Å skrive en tekstfil"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor 
Å skrive en tekstfil kan være et nødvendig skritt i å lære å programmere med Clojure. Det er også en viktig komponent i å lage fungerende programmer som kan håndtere ulike typer data.

## Hvordan å 
For å skrive en tekstfil med Clojure, kan du bruke funksjonen "spill-fil" eller "with-open". Her er et eksempel på hvordan du kan bruke "spill-fil" for å skrive en tekstfil:

```Clojure
(spill-fil "minfil.txt"
  (fårsindre [linje "Dette er en tekst"]
    (skriv-linje linje)))
```
Dette vil opprette en tekstfil som heter "minfil.txt" og legge til teksten "Dette er en tekst" på en ny linje i filen. Du kan også spesifisere en absolutt eller relativ bane for filen.

## Dypdykk 
Når du bruker "spill-fil" funksjonen, vil filen automatisk lukkes når skrivingen er ferdig. Men hvis du bruker "with-open" funksjonen, må du selv lukke filen ved hjelp av "lukk" funksjonen. Dette kan være nyttig hvis du trenger å gjøre flere operasjoner på filen.

En annen viktig del av skriving av tekstfiler er å kunne håndtere forskjellige typer data. Clojure har innebygde funksjoner som "skriv" og "skriv-linje" for å skrive ut numeriske verdier, strenger og andre datastrukturer som vektorer og kart.

## Se også 
- ["Brukerfiler med Clojure"](https://clojure.org/guides/io)
- ["Skape filhåndtering med Clojure"](https://www.braveclojure.com/files/)
- ["Innføring i Clojure for nybegynnere"](https://learnxinyminutes.com/docs/no-no/clojure-no/)