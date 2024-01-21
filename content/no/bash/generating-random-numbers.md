---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:48:38.289105-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall betyr å lage nummer som ikke har noe forutsigbart mønster. Programmerere bruker det for alt fra å teste kode til å skape spillmekanikker eller sikkerhetssystemer.

## Hvordan:
```Bash
# Generer et tilfeldig tall mellom 1 og 10
echo $((RANDOM % 10 + 1))

# Eksempel på utskrift
7
```

```Bash
# Generer et tilfeldig tall med /dev/urandom for større sikkerhet
echo $(( $(od -An -N2 -i /dev/urandom) % 10 + 1 ))

# Eksempel på utskrift
3
```

## Dypdykk
I Bash eksisterer det en innebygd variabel `$RANDOM` som gir et pseudotilfeldig tall mellom 0 og 32767. Å bruke `%` operatøren begrenser området, noe som er nyttig i mange scenarioer.

Historisk har tilfeldige tall fra datasystem kun vært pseudotilfeldige, noe som betyr at de er beregnet ved hjelp av en algoritme. Hvis du trenger mer sikre tilfeldige tall for kryptografi, er `/dev/urandom` et bedre valg siden den henter inndatadrevet "støy" for å produsere tallene.

Alternativer til `$RANDOM` kan inkludere `shuf`, `awk` eller til og med å installere eksterne verktøy som `rng-tools` for maskinvaregenerert entropi.

## Se Også
- `man bash` - for å lære mer om innebygde Bash-kommandoer.
- `man od` - for å forstå mer om `od` kommandoen for å lese binærfiler.
- [Diehard tests](https://en.wikipedia.org/wiki/Diehard_tests) - en serie av statistiske tester for å sjekke tilfeldigheten av tall.