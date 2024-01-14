---
title:    "Gleam: Generering av tilfeldige tall."
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle du bruke tilfeldig tall generering i din kodesnutt? Vel, det kan være mange grunner til det, enten du ønsker å legge til litt tilfeldighet i din applikasjon, generere unike ID-er, eller lage spill eller simuleringer. Uansett årsak, er generering av tilfeldige tall en nyttig funksjon å ha i verktøykassen din.

## Hvordan
For å generere tilfeldige tall i Gleam, kan du bruke standardbibliotekets `Math` modul. Denne modulen inneholder funksjoner for å generere tall av ulike typer, for eksempel heltall, desimaltall og boolske verdier. La oss ta en nærmere titt på hvordan man bruker disse funksjonene.

Først må du importere `Math` modulen i din kodesnutt:

```
import gleam/math
```

Deretter kan du kalle de ulike funksjonene ved å bruke navnromnotasjon:

- For å generere et heltall mellom et minimum og maksimumsverdi, kan du bruke `random_int()` funksjonen:

```
let random_number = gleam/math.random_int(1, 10)
```

Dette vil gi deg et tilfeldig tall mellom 1 og 10 (inkludert begge tallene).

- For å generere et desimaltall mellom 0 og 1, kan du bruke `random_float()` funksjonen:

```
let random_float = gleam/math.random_float()
```

- For å generere en tilfeldig boolsk verdi (sann eller falsk), kan du bruke `random_bool()` funksjonen:

```
let random_bool = gleam/math.random_bool()
```

## Dykk ned
Så hvordan fungerer egentlig tilfeldig tall generering i Gleam? Vel, `Math` modulen bruker en algoritme kalt Mersenne Twister for å generere tilfeldige tall. Denne algoritmen blir ofte brukt i andre programmeringsspråk og er kjent for å produsere tall med en høy grad av tilfeldighet.

Det er også verdt å merke seg at tilfeldig tall generering i Gleam er "seeds" -basert, noe som betyr at hver gang du kjører programmet ditt, vil du få de samme tilfeldige tallene. Hvis du ønsker å få forskjellige tall for hver kjøring, kan du bruke `random_seed()` funksjonen for å generere en ny "seed" hver gang.

## Se også
- [Dokumentasjon for Gleam Math modul](https://gleam.run/modules/math/0.9.0/)
- [Mersenne Twister algoritmen](https://en.wikipedia.org/wiki/Mersenne_Twister)

Takk for at du leste denne bloggposten om tilfeldig tall generering i Gleam. Vi håper den har hjulpet deg med å forstå hvordan du kan bruke denne funksjonen i dine prosjekter. Lykke til!