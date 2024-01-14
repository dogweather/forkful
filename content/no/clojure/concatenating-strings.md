---
title:    "Clojure: Sammenføyning av strenger"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Å slå sammen strenger er en vanlig oppgave i programmering, og kan være svært nyttig når man vil kombinere tekst og variabler for å lage dynamisk data. Det kan også hjelpe til med å organisere og strukturere data på en mer lesbar måte.

## Hvordan

For å slå sammen strenger i Clojure, kan du bruke funksjonen `str` som tar imot en eller flere strenger som argumenter. Her er et eksempel:

```Clojure
(str "Hei, " "verden!")
```

Dette vil resultere i en ny streng som sier `Hei, verden!`. Men du kan også inkludere variabler i denne funksjonen. For eksempel:

```Clojure
(def number 5)
(str "Det er " number " elefant(er) i skogen.")
```

Dette vil resultere i `Det er 5 elefant(er) i skogen.`

## Dypdykk

I Clojure er strenger uforanderlige, noe som betyr at de ikke kan endres etter at de er opprettet. Dette betyr at når du slår sammen strenger ved hjelp av `str`, så opprettes det faktisk en helt ny streng og de opprinnelige strengene forblir uendret.

Det finnes også flere andre funksjoner for å slå sammen strenger i Clojure, som `join` og `format`, som kan være nyttige i ulike situasjoner. Det er også viktig å være oppmerksom på at strenger i Clojure er Unicode-tekster, og derfor kan de inneholde mange forskjellige språk og symboler.

## Se også

- [Offisiell Clojure dokumentasjon](https://clojure.org/api/cheatsheet)
- [En enkel guide til Clojure strenger](https://www.freecodecamp.org/news/a-simple-guide-to-clojure-strings/)
- [Mer om Unicode og strenger i Clojure](https://clojure.org/guides/unicode)