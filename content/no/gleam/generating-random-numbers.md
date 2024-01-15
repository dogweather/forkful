---
title:                "Generering av tilfeldige tall"
html_title:           "Gleam: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Vi har alle en fascinasjon for tilfeldigheter, og generering av tilfeldige tall er en enkel måte å få en smakebit av det på. Enten det er for å lage et spill, simulere en situasjon eller bare for å tilføre litt variasjon i en rutine, er generering av tilfeldige tall nyttig og interessant.

## Hvordan
Generering av tilfeldige tall i Gleam er veldig enkelt og krever bare en enkelt linje med kode:
```Gleam
let random_number = Random.int()
```
Her genererer vi et tilfeldig heltall og lagrer det i en variabel kalt `random_number`. Vi kan også spesifisere et område for tallene ved å legge til to argumenter:
```Gleam
let random_number = Random.int(1, 10)
```
Dette vil generere et tilfeldig tall mellom 1 og 10. Vi kan også generere tilfeldige desimaler ved å bruke `Random.float()`. For å hente ut en tilfeldig verdi fra en liste, kan vi bruke `List.random()`:
```Gleam
let fruits = ["apple", "banana", "orange"]
let random_fruit = List.random(fruits)
```
Dette vil generere et tilfeldig element fra listen `fruits` og lagre det i variabelen `random_fruit`.

## Dykk dypere
Bak kulissene bruker Gleam Pseudorandom Number Generators (PRNGs) for å generere tallene. En PRNG er en algoritme som genererer en sekvens av tall som kan fremstå som tilfeldige, men er egentlig basert på en startverdi. Dette betyr at hvis vi angir den samme startverdien, vil den generere den samme sekvensen av tall. For å unngå dette, kan vi bruke `Random.seed()` for å generere en tilfeldig startverdi basert på klokka og systemtilfeldigheter.

## Se også
- [Gleam dokumentasjon om Random-modulen](https://gleam.run/modules/random/)
- [Wikipedia om Pseudorandom Number Generators (PRNGs)](https//en.wikipedia.org/wiki/Pseudorandom_number_generator)