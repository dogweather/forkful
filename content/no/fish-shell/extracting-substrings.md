---
title:                "Fish Shell: Utvinning av delstrenger"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Når vi jobber med tekstbehandling og manipulering i programmer, kan det være nyttig å kunne ekstrahere deler av en tekststreng basert på bestemte kriterier. Dette kan være for å finne og erstatte spesifikke ord, trekke ut data fra en større tekst eller lage nye variabler basert på eksisterende tekst. Å kunne ekstrahere substrings kan være en nyttig ferdighet å ha i verktøykassen din som programmerer.

## Hvordan du gjør det

For å ekstrahere substrings i Fish Shell, må vi bruke funksjonen `string sub`. Her er et eksempel på hvordan vi kan bruke denne funksjonen:

```Fish Shell
set tekst "Dette er en testtekst"
echo (string sub -s 6 -l 2 $tekst)
```

I dette eksempelet lar `string sub` oss velge startposisjonen og lengden på substringen vi ønsker å ekstrahere. I dette tilfellet begynner vi på posisjon 6 (som tilsvarer det første bokstavet i ordet "en") og trekker ut 2 bokstaver, som gir oss "en" som output.

Dette er en veldig enkel måte å ekstrahere substrings på, men man kan også bruke regex-uttrykk for å være mer spesifikk i utvelgelsen av substringer.

## Dypdykk

Dersom du ønsker å lære mer om å ekstrahere substrings i Fish Shell, kan du ta en titt på [`string sub` dokumentasjonen](https://fishshell.com/docs/current/cmds/string.html#string-sub) for å få en oversikt over alle tilgjengelige funksjoner og muligheter. Du kan også eksperimentere med forskjellige regex-uttrykk for å finne den beste måten å ekstrahere ønsket tekst på.

Et annet godt verktøy for å lære mer om substring-ekstraksjon er å lese og forstå hvordan forskjellige programmerer bruker denne funksjonen i sine prosjekter. Dette kan gi deg en bedre forståelse av hvordan du kan bruke det i egne prosjekter.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Regex-tutorial for nybegynnere](https://regexone.com/)
- [Eksempel på Fish Shell-prosjekt som bruker `string sub`](https://github.com/fish-shell/fish-shell/blob/master/share/functions/__fish_print_hostname.fish)