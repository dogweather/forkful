---
title:    "TypeScript: Å bruke regulære uttrykk"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regular Expressions, eller regulære uttrykk på norsk, er et viktig verktøy for å håndtere tekst og mønstergjenkjenning i programmering. Ved å lære å bruke regulære uttrykk i TypeScript, kan du enkelt finne, erstatte eller validere tekst basert på visse kriterier. Dette kan spare deg for mye tid og gjøre koden din mer effektiv.

## Hvordan

For å bruke regular expressions i TypeScript, må du først importere RegExp-objektet. Dette kan gjøres ved å skrive følgende kode:

```TypeScript
import { RegExp } from 'ts-regexp';
```

Nå kan du opprette et RegExp-objekt ved å gi det et uttrykk og valgfri flagg for å indikere hvordan uttrykket skal tolkes. La oss for eksempel lage et RegExp-objekt som skal finne alle ord som starter med en stor bokstav:

```TypeScript
let re = new RegExp('[A-Z]\\w+', 'g');
```

Her er `[A-Z]` uttrykket som indikerer at vi ønsker å finne en stor bokstav i starten av ordet, og `\\w+` betyr at vi ønsker å finne alle ord som følger etter denne store bokstaven. `g`-flagget betyr global, slik at alle samsvar blir funnet, ikke bare det første.

Når du har opprettet RegExp-objektet, kan du bruke forskjellige metoder for å utføre handlinger som å finne og erstatte tekst. For å finne tekst i en streng, kan du bruke `test()`-metoden som returnerer `true` eller `false` basert på om en match blir funnet:

```TypeScript
re.test('Dette er en tekst som vi ønsker å finne') // returnerer true
re.test('dette er en tekst som vi ikke ønsker å finne') // returnerer false
```

Du kan også bruke `match()`-metoden for å få en array med alle samsvar i en streng:

```TypeScript
'Teksten var full av store ord'.match(re) // returnerer ['Teksten', 'full']
```

Ved hjelp av `replace()`-metoden kan du også erstatte tekst som samsvarer med uttrykket ditt:

```TypeScript
'Teksten var full av store ord'.replace(re, 'Setninger') // returnerer 'Setninger var Setninger av store Setninger'
```

## Dypdykk

Regular Expressions i TypeScript har mange flere nyttige funksjoner og metoder som ikke er nevnt her. Selv om det kan virke forvirrende i begynnelsen, er det verdt å investere litt tid i å lære seg å bruke dem. De kan være svært nyttige i nesten alle programmeringsspråk og kan spare mye tid og innsats i det lange løp.

## Se Også

- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Official Website](https://www.typescriptlang.org/)
- [Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/typescript)