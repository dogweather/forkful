---
title:                "Bash: Generering av tilfeldige tall"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en viktig del av bash-programmering. Det kan være nyttig for å lage en tilfeldig spillfunksjon eller for å skape variasjon i et dataprogram. I denne bloggposten vil vi gå gjennom hvordan man kan generere tilfeldige tall i bash, og dykke dypere inn i konseptet bak dette.

## Hvordan lage tilfeldige tall i bash

For å generere tilfeldige tall i bash, kan du bruke kommandoen "RANDOM". Her er et eksempel på hvordan dette kan gjøres:

```Bash
#!/bin/bash
echo "Det tilfeldige tallet mitt er $((RANDOM%100))"
```
Dette vil generere et tilfeldig tall mellom 0 og 99 hver gang du kjører skriptet.

Du kan også bruke "shuf" kommandoen for å generere tilfeldige tall fra en gitt liste. For eksempel:

```Bash
#!/bin/bash
numbers=(1 2 3 4 5 6 7 8 9)
echo "Det tilfeldige tallet mitt er $(shuf -n 1 <<< "${numbers[@]}")"
```
Dette vil velge et tilfeldig tall fra listen og skrive det ut. Du kan også bruke "shuf" til å generere tilfeldige tall innenfor en gitt rekkevidde ved å bruke flagget "-i". For eksempel:

```Bash
#!/bin/bash
echo "Det tilfeldige tallet mitt er $(shuf -i 10-20 -n 1)"
```
Dette vil generere et tilfeldig tall mellom 10 og 20.

## Dypdykk

Når vi bruker kommandoen "RANDOM" eller "shuf" for å generere tilfeldige tall, er det viktig å huske at disse tallene ikke er helt tilfeldige. De er basert på en algoritme som genererer tall basert på et "frø" nummer, som er et tall som systemet velger. Dette betyr at hvis du for eksempel genererer et tilfeldig tall med "RANDOM" og deretter genererer et nytt tall i samme skript, vil det andre tallet være basert på det første tallet som et "frø". For å endre dette, kan du bruke kommandoen "RANDOM=$$".

Det er også viktig å merke seg at "RANDOM" bare genererer tilfeldige tall mellom 0 og 32767. Hvis du trenger større tall, kan du bruke "shuf" kommandoen som nevnt tidligere.

## Se også

- [Bash dokumentasjon om "RANDOM" kommandoen](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Bash dokumentasjon om "shuf" kommandoen](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [En dypere forklaring av tilfeldige tallgeneratorer](https://www.computerworld.com/article/3589113/what-are-random-numbers-and-how-do-we-get-them-2.html)