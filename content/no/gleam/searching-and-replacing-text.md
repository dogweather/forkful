---
title:                "Gleam: Søking og bytte av tekst"
simple_title:         "Søking og bytte av tekst"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Ved å søke og erstatte tekst i kode, kan du spare mye tid og redusere sjansene for feil. Det er en enkel og effektiv måte å endre flere linjer med kode på samtidig.

## Slik gjør du det

For å søke og erstatte tekst i Gleam, bruker du funksjonen `replace` sammen med et regulært uttrykk. Her er et eksempel på hvordan du kan søke etter alle forekomster av "hello" og erstatte det med "hei".

```Gleam
let nyTekst = replace("hello", ~flags=~global, "hei", "hello world")
```

Dette koden vil gi deg outputen "hei world", hvor "hello" er erstattet med "hei". Som du kan se, bruker vi `replace` funksjonen sammen med flagget `~global` for å sikre at alle forekomster av "hello" blir erstattet. Du kan også bruke et regulært uttrykk i stedet for å erstatte en konkret tekststreng.

## Gå dypere

I Gleam kan du også bruke en callback-funksjon i `replace` for mer avanserte søk og erstatninger. Dette gjør det mulig å gjøre forskjellige operasjoner avhengig av teksten som blir funnet. Her er et eksempel på en callback-funksjon som tar i mot en tekststreng og returnerer en verdi basert på den:

```Gleam
let callback = fn
  (tekst) ->
    let verdi = // gjør noe med tekststrengen
    verdi

let nyTekst = replace("hello", ~flags=~global, callback, "hello world")
```

Som du kan se, kan du bruke callback-funksjonen til å gjøre forskjellige operasjoner basert på teksten som blir funnet. Dette gir deg en mer fleksibel og kraftig måte å søke og erstatte tekst på i koden din.

## Se også

- [Gleam dokumentasjon](https://gleam.run/book/stdlib.html#replace)
- [Regulære uttrykk i Gleam](https://gleam.run/book/types.html#regexp)