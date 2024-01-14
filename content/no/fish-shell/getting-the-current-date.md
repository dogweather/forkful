---
title:    "Fish Shell: Å få dagens dato"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen bry seg om å få den nåværende datoen? Vel, det er faktisk ganske nyttig når du jobber med programmering eller administrasjon av datamaskiner. Å vite den eksakte datoen kan være nyttig for å automatisere oppgaver eller sjekke filegenskaper.


## Hvordan

For å få den nåværende datoen i Fish Shell, kan du bruke kommandoen "date". La oss se på et eksempel:

```
Fish Shell:

$ date
torsdag, 24. juni 2021, 14:00:00 CEST
```

Som du kan se, gir kommandoen oss den nåværende datoen i et leselig format. Men hva om du vil ha datoen i et annet format? Ikke noe problem, du kan enkelt formatere utdataen ved å legge til parameterne "+%format" etter "date" kommandoen. La oss si at du vil ha datoen i formatet MM-DD-ÅÅÅÅ. Her er hvordan du gjør det:

```
Fish Shell:

$ date +%m-%d-%Y
06-24-2021
```

Det er det! Nå har du datoen i ønsket format. Det er også mange andre formateringsalternativer tilgjengelig, som for eksempel å vise bare måneden eller å legge til kommateringer. Du kan lese mer om disse mulighetene i Fish Shell dokumentasjon.

## Dykk dypere

Visste du at "date" kommandoen også kan brukes til å sette datoen? Det stemmer, du kan bruke den til å endre den nåværende datoen på datamaskinen din. Du trenger bare å legge til "MMDDhhmmyyyy" etter "date" kommandoen for å sette ønsket dato. Det kan være nyttig for å teste programmer eller for å tilbakestille datoen etter en feil. Men vær forsiktig og bruk dette bare hvis du vet hva du gjør!

## Se også

- Fish Shell dokumentasjon: https://fishshell.com/docs/current/index.html
- "Hvordan formatere dato og klokkeslett i Fish Shell": https://blog.virtualbits.io/how-to-format-datetime-in-fish-shell/