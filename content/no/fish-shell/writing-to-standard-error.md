---
title:                "Skrive til standardfeil"
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving til standardfeil (`stderr`) handler om å sende feilmeldinger og diagnostikk separat fra hoveddatautstrømmen (`stdout`). Dette gjøres for å gjøre det enklere å skille mellom vanlig output og feil, noe som er nyttig for feilsøkning og logging.

## Hvordan:
```Fish Shell
echo "Dette er en melding til standard output"  # Normal output
echo "Dette er en feilmelding" >&2  # Melding til standard error

# Eksempel på bruk der standard output og error skilles:
echo "Vanlig output" > output.txt
echo "Feilmelding" >&2 > error.txt

# Forventet output til terminal:
# Feilmelding
# Innholdet i 'output.txt' vil være "Vanlig output"
# 'error.txt' vil ha ingen data siden den ble omdirigert fra stderr som er skrevet direkte til terminalen
```

## Dypdykk
Historisk sett kommer skille mellom `stdout` og `stderr` fra Unix-tiden, slik at programmer kunne sende data og feil til forskjellige strømmer. I Fish kan du omdirigere `stderr` med `>&2`. Alternativt, kan du omdirigere både standard output og error til samme sted med `&> filnavn`.

Skille dem er viktig når du rørlegger kommandoer. For eksempel, `kommando1 | kommando2` vil sende `kommando1` sin `stdout` til `kommando2`, men ikke `stderr`. For å inkludere feil i røret, bruk `kommando1 ^| kommando2`. Details om implementasjon kan avhenge av operasjonssystemet og skallets versjon.

## Se Også
- Fish dokumentasjon om omdirigering: https://fishshell.com/docs/current/index.html#redirection
- Unix programmeringsveiledning: http://www.tldp.org/LDP/abs/html/io-redirection.html
- Feilsøking i Fish Shell: https://fishshell.com/docs/current/index.html#debugging