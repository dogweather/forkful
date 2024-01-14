---
title:                "Bash: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor

Å generere tilfeldige tall er en vanlig oppgave når du arbeider med Bash programmering. Det kan være nyttig for å skape unike identifikatorer, teste tilfeldige scenarier eller simulere tilfeldige hendelser. Uansett formål, gir Bash et enkelt og pålitelig måte å generere tilfeldige tall på.

# Hvordan

Det første trinnet er å bruke `$$` variabelen til å få tak i prosessidentifikatoren (PID) for den nåværende Bash-prosessen. Dette er en tallverdi som endres hver gang Bash kjører et nytt program. Vi kan bruke dette tallet som en "frø" for å få forskjellige tall hver gang vi kjører programmet.

```
Bash
echo $$
```
> Output: 29586

For å generere et tilfeldig tall, kan vi bruke `RANDOM` variabelen som genererer et tilfeldig tall mellom 0 og 32767 hver gang den brukes.

```
Bash
echo $RANDOM
```
> Output: 29201

Vi kan kombinere disse to variablene for å generere større tilfeldige tall.

```
Bash
echo $(($$+$RANDOM))
```
> Output: 60765

Vi kan også bruke `shuf` kommandoen til å generere tilfeldige tall innenfor et bestemt område. I eksempelet nedenfor genererer vi ett tilfeldig tall mellom 1 og 100.

```
Bash
shuf -i 1-100 -n 1
```
> Output: 73

# Dykk dypere

I Bash, er tilfeldige tall generert ved hjelp av en pseudorandom generator. Dette betyr at tallene er basert på en bestemt matematisk formel og er ikke helt tilfeldige.

Hvis du vil bruke tilgangen `RANDOM` variabel til å generere en sekvens av tilfeldige tall, kan du bruke `srand()` og `rand()` funksjoner. Dette lar deg gi en "frø" verdi og kontrollere sekvensen av tall som blir generert.

# Se også

- [Bash dokumentasjon: RANDOM](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#index-RANDOM)
- [Artikkel: Generer tilfeldige tall i Bash](https://www.baeldung.com/linux/bash-generate-random-number)
- [Artikkel: Bash tilfeldige tall-triks](https://linuxhandbook.com/bash-random-number/)