---
title:                "Javascript: Søking og utskifting av tekst"
simple_title:         "Søking og utskifting av tekst"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bytte ut tekst i et prosjekt kan være en viktig del av å forbedre koden din, organisere informasjonen din eller gjøre batch-oppdateringer. Ved å bruke JavaScript, kan du enkelt utføre søk og erstatninger i tekstfiler. Dette kan hjelpe deg med å spare tid og forbedre effektiviteten din som utvikler.

## Hvordan

For å utføre en søk og erstatning i en tekstfil med JavaScript, kan du bruke metoden `replace()`. Her er et eksempel på hvordan du kan erstatte et ord i en tekst med et annet ord:

```Javascript
let tekst = "Jeg elsker å kode i JavaScript!";
let nyTekst = tekst.replace("elsker", "digger");
// nyTekst vil nå være "Jeg digger å kode i JavaScript!"
```

Du kan også bruke regulære uttrykk for å gjøre enda mer komplekse søk og erstatninger. Her er et eksempel på hvordan du kan erstatte alle små bokstaver i en tekst med store bokstaver:

```Javascript
let tekst = "dette er en tekst med små bokstaver.";
let nyTekst = tekst.replace(/[a-z]/g, match => match.toUpperCase());
// nyTekst vil nå være "DETTE ER EN TEKST MED SMÅ BOKSTAVER."
```

Som du kan se, bruker vi her `replace()`-metoden kombinert med et regulært uttrykk for å matche alle små bokstaver og deretter bytte dem ut med tilsvarende store bokstaver.

## Dypdykk

Det er viktig å merke seg at `replace()`-metoden bare vil erstatte det første tilfellet av søket i teksten. Hvis du ønsker å erstatte alle forekomster, må du bruke et regulært uttrykk med flagget `g` (global).

Det er også viktig å ta hensyn til forskjellen mellom datatypene `string` og `RegExp` når du bruker regulære uttrykk. Du kan ikke bruke `match()`-metoden på en variabel av datatypen `string`, så du må sørge for å alltid bruke regulære uttrykk når du skal utføre søk og erstatning.

## Se også

- [MDN Web Docs: replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)