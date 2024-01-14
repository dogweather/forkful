---
title:    "Fish Shell: Konvertere en streng til små bokstaver"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er en vanlig oppgave i programmering. Det kan være nyttig for å sikre konsistens i data, filtrere og søke gjennom tekst, eller generelt sett forbedre lesbarheten og estetikken i koden din.

## Hvordan

For å konvertere en streng til små bokstaver i Fish Shell bruker du kommandoen `string tolower (streng)`. Her er et enkelt eksempel:

```
Fish Shell:
string tolower "HELLO WORLD"
```

Dette vil gi følgende utdata:

```
hello world
```

Du kan også bruke `string tolower` på variabler eller output fra andre kommandoer. For eksempel, hvis du ønsker å konvertere output fra kommandoen `echo` til små bokstaver, kan du skrive:

```
Fish Shell:
string tolower (echo "HELLO WORLD")
```

Dette vil også gi utdataen `hello world`.

## Dypdykk

Det er viktig å være oppmerksom på at `string tolower` kun fungerer på ASCII-tegn. Dette betyr at noen spesielle tegn i andre språk kan bli påvirket på uventede måter. En måte å unngå dette på er å bruke Unicode-baserte funksjoner som `string lower` og `string normal`. Disse funksjonene tar hensyn til språkspesifikke regler for å konvertere bokstaver til små bokstaver.

## Se også

- [Fish Shell dokumentasjon for string manipulation](https://fishshell.com/docs/current/index.html#string-manipulation)
- [Unicode Table](https://unicode-table.com/) for å se forskjellen mellom ASCII og Unicode-tegn.