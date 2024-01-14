---
title:    "Gleam: Utenpåhogging av substringer"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut delstrenger er ofte en viktig del av programmering, spesielt når man arbeider med tekstbehandling og parsing av data. Det kan være nødvendig å isolere bestemte deler av en streng for å kunne utføre operasjoner eller sammenligne data. I Gleam, et funktionelt programmeringsspråk for moderne utviklere, er det en enkel måte å trekke ut delstrenger på, som vil bli forklart i denne bloggposten.

## Hvordan

For å trekke ut en delstreng i Gleam, bruker man funksjonen `string.split_at()` sammen med indekseringsoperatøren (`[]`). Denne funksjonen tar to argumenter: en streng og en indeks, og returnerer to delstrenger - en som viser strengen fra starten frem til indeksen, og en som viser strengen fra indeksen og ut. La oss bruke et eksempel for å forklare bedre.

```Gleam
let streng = "Husky Valper"
let (første_ord, siste_ord) = string.split_at(streng, 5)

// Første ord blir "Husky" og siste ord blir "Valper"
```

Som du kan se, var `første_ord` og `siste_ord` de to delstrengene som ble returnert fra funksjonen `string.split_at()`.

## Dypdykk

Det er også mulig å trekke ut flere delstrenger ved å bruke `string.split_at()` flere ganger. La oss se på et annet eksempel:

```Gleam
let streng = "1-2-3-4-5"
let (første_tall, resten) = string.split_at(streng, 1)

let (andre_tall, endelig) = string.split_at(resten, 1)

// Første tall blir "1", andre tall blir "2" og endelig blir "3-4-5"
```

Som du kan se, brukte vi `string.split_at()` to ganger for å isolere de tre tallene fra strengen. Først delte vi strengen ved tegnet "-" for å få den første delen, og deretter delte vi resten av strengen ved samme tegn for å få de to siste tallene.

## Se også

- [Offisiell Gleam dokumentasjon om strengebehandling](https://gleam.run/documentation/#strings)
- [Gleam sin offisielle nettside](https://gleam.run/)
- [Andre nyttige ressurser og eksempler på Gleam-prosjekter](https://github.com/gleam-lang/awesome-gleam)