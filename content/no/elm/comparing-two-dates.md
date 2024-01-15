---
title:                "Sammenligning av to datoer"
html_title:           "Elm: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor
Å sammenligne to datoer er en vanlig oppgave i mange programmeringsprosjekter. Dette er spesielt nyttig når du trenger å sjekke om en dato kommer før eller etter en annen, eller om de to datoene er like. I denne artikkelen vil vi ta en titt på hvordan du kan sammenligne datoer ved hjelp av Elm-programmeringsspråket. 

# Hvordan
For å sammenligne to datoer i Elm, kan du bruke funksjonen `compare`. Denne funksjonen tar inn to datoer og returnerer en `Order`-verdi, som kan være `LT` (less than), `EQ` (equal) eller `GT` (greater than). La oss se på et eksempel der vi sammenligner to datoer:

```Elm
compare (Date.fromParts 2020 10 20) (Date.fromParts 2020 10 25)
```

Her vil funksjonen returnere `LT`, siden 20. oktober kommer før 25. oktober.

Du kan også sammenligne datoen din med dagens dato ved å bruke `Date.today`. La oss se på et annet eksempel der vi sjekker om en dato er før eller etter dagens dato:

```Elm
Date.compare (Date.fromParts 2020 10 31) Date.today
```

Dette vil returnere `GT`, siden 31. oktober kommer etter dagens dato.

# Dypdykk
Når du sammenligner to datoer, er det viktig å merke seg at `compare`-funksjonen tar hensyn til både dato og tid. Dette betyr at hvis du ønsker å sammenligne to datums, må de også ha samme tidspunkt. Hvis du ikke er interessert i tid, kan du bruke funksjonen `Date.toMidnight` for å sette tiden til midnatt.

Videre kan du også bruke `Date.isEqual` og `Date.isBefore` for å sjekke om to datoer er like eller om en dato kommer før en annen. Disse funksjonene returnerer en `Bool`-verdi, som kan være `True` eller `False`.

# Se Også
- [Elm Offisiell Dokumentasjon](https://guide.elm-lang.org/)
- [Elm Forum](https://discourse.elm-lang.org/)