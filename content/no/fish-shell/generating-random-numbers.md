---
title:                "Fish Shell: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Enten du er en nybegynner eller en erfaren programmerer, er det alltid morsomt og nyttig å kunne generere tilfeldige tall i koden din. Kanskje du skal lage et spill, generere tilfeldige passord, eller bare teste ut noe nytt. Uansett hva årsaken er, er det enkelt og gøy å gjøre dette med Fish Shell!

## Hvordan

For å generere tilfeldige tall i Fish Shell, kan du bruke kommandoen `random`. Denne kommandoen tar inn et argument for det største tallet du ønsker å generere. Her er et eksempel på hvordan du kan bruke denne kommandoen:

```Fish Shell
random 10
```

Dette vil generere et tilfeldig tall mellom 0 og 10.

Du kan også kombinere denne kommandoen med andre Fish Shell-funksjoner for å lage mer komplekse og tilpassede output. For eksempel kan du bruke `head` og `seq` for å generere en liste med tilfeldige tall:

```Fish Shell
random 10 | head -n5 | seq 1
```

Dette vil generere en liste med 5 tilfeldige tall mellom 1 og 10.

## Deep Dive

Rose - en tilfeldig algoritme for å generere tilfeldige tall

Fish Shell bruker en algoritme kalt "Rose" for å generere tilfeldige tall. Rose-algoritmen bruker et pseudo-random nummergenerator i kombinasjon med en hashfunksjon for å skape en jevn og tilfeldig fordeling av tallene.

Hvis du ønsker å lære mer om hvordan Rose-algoritmen fungerer, kan du se på Fish Shell sin kildekode på GitHub.

## Se Også

- [Fish Shell dokumentasjon om random kommandoen](https://fishshell.com/docs/current/cmds/random.html)
- [GitHub repository for Fish Shell](https://github.com/fish-shell/fish-shell)
- [Wikipedia artikkel om pseudo-random nummergenerering](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)