---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å analysere en dato fra en streng betyr å konvertere en tekst som representerer en dato til et standard datatidsformat. Programmerere gjør dette for enklere håndtering og manipulering av datoer. 

## Slik Gjør Du Det:

For å analysere en dato i Fish Shell, bruk `date`-kommandoen med `-u`-flagget og skriv datoen som argument. Her er et eksempel:

```fish
set -l date_string "2022-01-01 00:00:00"
set -l parsed_date (date -u -j -f "%Y-%m-%d %T" $date_string "+%s")
echo $parsed_date
```

Koden over vil gi uttaket `1640995200`, som er representasjonen av "2022-01-01 00:00:00" i Unix-tidsstempel.

## Dyp Dykk

Analysere datoer fra strenger har blitt en vanlig oppgave i programmering ettersom det letter datatids-håndtering. I skriptspråk som Fish Shell har dette blitt enda enklere takket være innebygde funksjoner som `date`.

Alternativt kan du også analysere en dato i Fish Shell ved hjelp av `strptime`-funksjonen, men den er mer kompleks og manuell. 

Når det gjelder implementasjon, bruker `date`-kommandoen i Fish Shell `getdate`-funksjonen fra glibc, og formatstrengen følger samme format som `strftime`-funksjonen.

## Se Også:

For mer informasjon om analyse av datoer i Fish Shell, vennligst se følgende kilder:

- [Fish Shell Dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub](https://github.com/fish-shell/fish-shell)
- [StackOverflow: Parsing Dates in Fish Shell](https://stackoverflow.com/questions/65935528/parsing-dates-in-fish-shell)