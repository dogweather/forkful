---
title:                "Utvinning av delstrenger"
html_title:           "Gleam: Utvinning av delstrenger"
simple_title:         "Utvinning av delstrenger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Du har kanskje hørt begrepet "utvinne substrings" og lurt på hva det egentlig betyr og hvorfor noen ville gjøre det. Kort sagt, å utvinne substrings handler om å hente ut en del av en tekst eller en tekstvariabel basert på et bestemt kriterium. Dette kan være nyttig når du jobber med tekstbehandling eller dataanalyse, og det kan gjøre koden din mer effektiv og lesbar.

## Hvordan å

For å utvinne substrings i Gleam, bruker du funksjonen `String.sub()`. Denne funksjonen tar tre argumenter: en tekst, en startindeks og en sluttkildeks. Den returnerer en ny tekst som består av en del av den originale teksten basert på start- og sluttposisjonene du angir.

La oss si at vi har følgende tekstvariabel:

```Gleam
tekst = "Hei, hva skjer?"
```

Hvis vi bare vil ha ordet "hva" fra denne teksten, bruker vi `String.sub()`:

```Gleam
String.sub(tekst, 5, 7)
```

Dette vil returnere `hva`, som er ordet mellom indeks 5 og 7 i teksten. Du kan også bruke negative tall for å angi indekser fra slutten av teksten. For eksempel, hvis du vil ha de to siste tegnene i teksten, kan du bruke:

```Gleam
String.sub(tekst, -2, -1)
```

Dette vil returnere `er`, siden det er de to siste tegnene i teksten. Hvis du bare angir en startindeks vil funksjonen returnere en del av teksten fra og med denne indeksen til slutten av teksten. Og hvis du bare angir en sluttkildeks, vil funksjonen returnere en del av teksten fra starten til denne indeksen.

## Dypdykk

Som nevnt kan du bruke både positive og negative tall for å angi indekser når du bruker `String.sub()`. Men du kan også bruke variabler eller beregninger som argumenter, for eksempel:

```Gleam
start = 10
slutt = 15
String.sub(tekst, start, slutt)
```

Eller:

```Gleam
tall = 2 + 3
String.sub(tekst, 0, tall)
```

Dette vil hjelpe deg med å dynamisk hente ut substrings basert på varierende kriterier. Du kan også bruke funksjonen `String.len()` for å få lengden på en tekst og bruke det som en sluttkildeks for å få den siste delen av teksten.

## Se Også

- Offisiell Gleam dokumentasjon for `String.sub()`: https://gleam.run/guides/strings.html#string-substring
- En annen artikkel om å utvinne substrings i Gleam: https://medium.com/@sle/don-t-like-slicing-use-string-sub-in-gleam-c9de602fc6f5