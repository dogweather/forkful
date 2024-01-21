---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:48:57.366698-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall er å lage nummer som ikke kan forutsies. Programmerere gjør det for å sikre variasjon, rettferdighet i spill, sikkerhet i kryptering og for å simulere data for tester.

## Slik gjør du:
For å generere et tilfeldig tall i Fish, bruk `random`-kommandoen. Her er noen eksempler:

```Fish Shell
# Generer et tilfeldig tall mellom 1 og 10
set -l tilfeldig_tall (random 1 10)
echo $tilfeldig_tall
```
Output: `7` (din utskrift vil variere)

```Fish Shell
# Generer et tilfeldig tall og lagre det i en variabel
set -l hemmelig_nummer (random)
echo $hemmelig_nummer
```
Output: `1532483297` (din utskrift vil variere)

```Fish Shell
# Generer en rekke med tilfeldige tall
random --number=5
```
Output: `1987402347 321424578 2364732891 1723746890 3429815604` (din utskrift vil variere)

## Dypdykk
Tilfeldige tall i databehandling har vært sentralt fra starten, kritisk for simuleringer og sikkerhet. `random` i Fish er en bygget inn kommando, basert på pseudotilfeldige nummer generatorer (PRNGs). Alternativer som `/dev/random` kan brukes i andre skall, men Fish sin innebygde `random` er både enkel å bruke og rask.

PRNGs er deterministiske, de trenger en 'seed' for å starte generasjonen. Uten en ny 'seed' vil PRNGs gi samme sekvens av tall. Fish's `random` håndterer seeding automatisk, så du får forskjellige tall hver gang.

Fish's `random` bruker en kvalitet PRNG, men husk at for ekstremt sensitivt bruk, som kryptografi, bør du kanskje se etter spesialiserte kryptografiske verktøy, som gir enda sterkere tilfeldighetsegenskaper.

## Se også
- Fish's offisielle dokumentasjon om `random`: https://fishshell.com/docs/current/cmds/random.html
- Wikipedia om pseudotilfeldige nummer generatorer: https://no.wikipedia.org/wiki/Pseudotilfeldige_tall
- 'random' kommando diskusjon på Fish's GitHub: https://github.com/fish-shell/fish-shell/issues/organize?q=is%3Aissue+random