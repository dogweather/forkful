---
title:    "Bash: Generering av tilfeldige tall"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en viktig del av programmering og kan brukes i ulike sammenhenger. Det kan for eksempel være nyttig når du trenger å lage ulike testdata, eller for å lage et tilfeldig valg i et spill eller en app.

## Hvordan gjøre det i Bash

For å generere tilfeldige tall i Bash, kan du bruke et innebygd kommando som heter `RANDOM`. Denne kommandoen genererer et tilfeldig tall mellom 0 og 32767 hver gang den blir kalt. Du kan bruke denne kommandoen på flere måter, avhengig av hva du skal bruke de tilfeldige tallene til.

For å generere et enkelt tilfeldig tall, kan du bruke følgende kommando:

```Bash
echo $(( RANDOM % 10 ))
```

Dette vil gi deg et tall mellom 0 og 9. Hvis du trenger et større tall, så kan du endre tallet etter prosenttegnet. For eksempel vil `echo $(( RANDOM % 100 ))` generere et tall mellom 0 og 99.

Hvis du trenger et tilfeldig tall innenfor et bestemt område, for eksempel mellom 10 og 20, kan du bruke følgende kommando:

```Bash
echo $(( RANDOM % 11 + 10 ))
```
Dette vil gi deg et tall mellom 10 og 20. Det første tallet (11) er antall tall i området, mens det andre tallet (10) er starten på området.

Du kan også generere flere tilfeldige tall samtidig, ved å bruke en løkke. Her er et eksempel på å generere 10 tilfeldige tall mellom 1 og 100:

```Bash
for i in {1..10}
do
  echo $(( RANDOM % 100 + 1 ))
done
```

## Dypdykk

Selv om kommandoen `RANDOM` er enkel å bruke og kan gi deg tilfeldige tall, så er det viktig å merke seg at tallene ikke er helt tilfeldige. Kommandoen bruker en algoritme for å generere tallene, og det er mulig å forutsi og manipulere resultatene. Dette er ikke et stort problem for de fleste bruksområder, men hvis du trenger helt tilfeldige tall, bør du bruke et annet verktøy eller en annen programmeringsspråk for å generere dem.

Det finnes også andre måter å generere tilfeldige tall på i Bash, som for eksempel ved å bruke funksjonen `rand()` i `awk`. Dette er et mer avansert alternativ, men kan være nyttig hvis du trenger større kontroll over genereringen av tallene.

## Se også

- [Random numbers in Bash](https://www.shell-tips.com/bash/random-numbers/)
- [Random numbers in awk](https://www.gnu.org/software/gawk/manual/gawk.html#Builtin-Functions-for-Random-Numbers)
- [True random numbers](https://www.random.org/)