---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall er prosessen med å produsere en sekvens av tall som ikke kan forutsies på noen bedre måte enn ved ren tilfeldighet. Programmerere gjør dette for å sørge for unikalitet eller pseudotilfeldighet i ulike anvendelser, for eksempel i spill, simuleringer og kryptering.

## Hvordan:

Bash tilbyr en intern variabel `$RANDOM` for å generere tilfeldige tall. Her er noen enkle bruksområder:

```Bash
# Enkelt tilfeldig tall mellom 0 og 32767
echo $RANDOM
```

Utskrift kan variere da tallet er tilfeldig, men det vil være et tall mellom 0 og 32767.

For å begrense det tilfeldige tallet til et spesifikt område, bruk modulusoperatøren `%`.

```Bash
# Tilfeldig tall mellom 0 og 99
echo $((RANDOM%100))
```

## Dyp Dykk: 

Generering av tilfeldige tall har en lang historie i databehandling, og mange metoder har blitt utviklet. `$RANDOM` i Bash, er faktisk et pseudotilfeldig tall, det vil si det er generert av en deterministisk prosess, men ser tilfeldig ut. 

Det er også alternative metoder til `$RANDOM` for å generere tilfeldige tall i Bash, for eksempel ved hjelp av spesielle steder i systemet som `/dev/urandom` eller `/dev/random`. De benytter miljøstøy samlet fra enhetsdrivere og andre kilder for å produsere tilfeldige tall.

```Bash
# Generer et tilfeldig tall ved hjelp av /dev/urandom
od -A n -t d -N 2 /dev/urandom | awk '{print $1}'
```

Det er viktig å merke seg at forskjellige metoder kan ha forskjellige egenskaper, både i forhold til ytelse og kvaliteten på tilfeldigheten. 

## Se Også:

For mer informasjon om generering av tilfeldige tall i Bash og alternativer til `$RANDOM`, sjekk ut følgende ressurser:

- Bash Handbook: [https://github.com/denysdovhan/bash-handbook](https://github.com/denysdovhan/bash-handbook)
- Artikkel på LinuxConfig.org om bruk av `/dev/random` og `/dev/urandom`: [https://linuxconfig.org//random-vs-urandom](https://linuxconfig.org//random-vs-urandom)