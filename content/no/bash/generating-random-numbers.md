---
title:                "Generering av tilfeldige tall"
html_title:           "Bash: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Det finnes mange situasjoner hvor du kan trenge å generere tilfeldige tall i Bash-programmering. Dette kan for eksempel være for å lage tilfeldig data til testing, lage et spill eller for å generere et passord.

## Hvordan

Det finnes to hovedmetoder for å generere tilfeldige tall i Bash - `$RANDOM`-variabelen og `shuf`-kommandoen. Her er noen eksempler på hvordan du kan bruke disse:

```Bash
# Genererer et tilfeldig tall mellom 0 og 100:
echo $((RANDOM % 101))

# Lager et tilfeldig passord med 8 tegn:
passord=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | head -c 8)
echo $passord

# Lagrer en listen med 5 tilfeldige tall i en fil:
shuf -i 1-100 -n 5 > tall.txt
cat tall.txt
```

Eksempel på output:

```
74
jKc3Gt4D
20
95
75
```

## Dypdykk

Både `$RANDOM`-variabelen og `shuf`-kommandoen bruker en pseudorandomiseringsalgoritme for å generere tilfeldige tall. Dette betyr at tallene ikke er helt tilfeldige, men følger en bestemt algoritme.

`$RANDOM`-variabelen genererer tall mellom 0 og 32767, mens `shuf`-kommandoen lar deg spesifisere et intervall ved hjelp av `-i` flagget.

Hvis du trenger å generere kryptografisk sikre tilfeldige tall, bør du heller bruke `openssl rand` kommandoen i stedet.

## Se også

- [The Bash Manual on RANDOM Variable](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Variables)
- [The Bash Manual on shuf command](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Builtins)
- [The Bash Manual on openssl command](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Redirections)