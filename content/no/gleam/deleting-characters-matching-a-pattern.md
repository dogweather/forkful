---
title:    "Gleam: Sletting av tegn som matcher et mønster."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster kan være en nødvendig oppgave for å rydde og organisere data. Det kan også være nyttig for å filtrere ut uønskede tegn i en tekststreng.

## Hvordan

For å slette tegn som matcher et mønster i Gleam må vi først importere biblioteket `Regex` og bruke funksjonen `Regex.replace`. Her er et eksempel på hvordan dette kan gjøres:

```Gleam
import Regex
tekst = "Hei, jeg heter Ole! Velkommen til min blogg."
mønster = Regex.compile("[A-Z]")
Regex.replace(mønster, tekst, "")
```

Dette vil resultere i teksten `"ei, jeg heter le! elkommen til min blogg."` da alle store bokstaver er blitt slettet. Merk at vi måtte bruke en regular expression (mønster) for å spesifisere hvilke tegn vi ønsket å slette. Det er også mulig å bruke variabler for å gjøre mønsteret mer dynamisk og allsidig.

## Deep Dive

Det er viktig å forstå hvordan en regular expression fungerer for å kunne slette riktig type tegn. En regular expression er en streng som følger et spesifikt syntaksmønster, og brukes til å finne mønstre i en tekststreng. I Gleam ender alle regular expressions med `Regex.t` for å indikere at det er et regex-objekt. 

Det finnes mange ulike syntakselementer i regular expressions, blant annet `.` som symboliserer et hvilket som helst tegn, `*` som betyr at det forrige elementet kan forekomme null eller flere ganger, og `[]` som brukes til å definere hvilke tegn som skal matche. Det finnes også en rekke spesifikke syntakselementer for ulike typer tegn, f.eks. `\d` for tall og `\s` for mellomrom.

Det er viktig å lese dokumentasjonen for å forstå hvordan man kan bruke de ulike syntakselementene til å lage et effektivt mønster for å slette tegn.

## Se Også
- [Gleam dokumentasjon for Regex](https://gleam.run/stdlib/regex.html)
- [En guide til regulære uttrykk (på norsk)](https://docs.oracle.com/cd/E42724_01/regex/docs/introduction.html)