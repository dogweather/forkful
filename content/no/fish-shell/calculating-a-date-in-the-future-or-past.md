---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Fish Shell: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig i forskjellige situasjoner, for eksempel når du planlegger en ferie eller jobber med tidsfølsomme oppgaver. Ved hjelp av Fish Shell kan du enkelt utføre slike beregninger uten å måtte bytte til en annen app eller nettsted.

## Slik gjør du det

For å beregne en dato i fremtiden eller fortiden i Fish Shell, kan du bruke «cal» kommandoen. Den bruker følgende syntaks:

```fish
cal [NUM] [UNIT] [+-] [NUM] [UNIT]
```

Her er en liste med noen eksempler på hvordan du kan bruke denne kommandoen:

```fish
cal 5 days ahead
cal 2 weeks ago
cal 10 years ahead
```

Output vil være i form av en kalender, som viser datoen beregnet basert på ditt nåværende systemtid.

## Dypdykk

For å forstå hvordan «cal» kommandoen fungerer, kan du se nærmere på de ulike delene av syntaksen. «NUM» står for antall og kan være en hvilken som helst numerisk verdi. «UNIT» refererer til en tidsenhet, for eksempel days, weeks eller years. Dette bestemmer hvor stor forskjell det skal være mellom din nåværende dato og den beregnede datoen.

«+-» angir om du vil beregne en dato i fremtiden (+) eller fortiden (-). Ved å kombinere ulike verdier av NUM og UNIT, kan du enkelt tilpasse beregningene etter dine behov.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Tutorial for å komme i gang med Fish Shell](https://www.freecodecamp.org/news/a-beginners-guide-to-fish-the-friendliest-shell/)
- [Slik kan du enkelt tilpasse Fish Shell](https://www.maketecheasier.com/customize-fish-shell/)