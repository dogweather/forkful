---
title:                "Generere tilfeldige tall"
html_title:           "Fish Shell: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Generering av tilfeldige tall er en vanlig oppgave for programmerere. Det er en måte å lage ulike resultat hver gang et program kjøres. Programmerere gjør dette for å skape variasjon og tilfeldighet i sine programmer.

## Hvordan:
Fish Shell har innebygd funksjonalitet for å generere tilfeldige tall. For å lage et tilfeldig tall mellom 1 og 10, kan du bruke kommandoen ```fish_random 1 10```. Resultatet vil da bli en tilfeldig verdi mellom 1 og 10.

Et annet nyttig verktøy er kommandoen ```fish_random-string```, som genererer en tilfeldig streng på 16 tegn.

## Dykk dypere:
Generering av tilfeldige tall har vært en viktig del av programmering i lang tid. Tidligere måtte programmere bruke komplekse matematiske algoritmer for å få tilfeldige tall. I dag har de fleste programmeringsspråk, inkludert Fish Shell, innebygd funksjonalitet for å gjøre dette enkelt.

En alternativ metode for å generere tilfeldige tall er å bruke en tilfeldig tallgenerator på en annen kilde, for eksempel et eksternt nettsted eller en fysisk enhet som en terning. Dette kan være nyttig hvis du trenger ekstra sikkerhet og tilfeldighet i dine tilfeldige tall.

For de som er interessert i implementeringen av tilfeldige tallgenerering, fungerer Fish Shell ved å bruke en pseudorandom generator. Dette er en matematisk algoritme som beregner et tilfeldig tall basert på en startverdi.

## Se også:
- [Fish Shell dokumentasjon om tilfeldige tallgenerering](https://fishshell.com/docs/current/cmds/fish_random.html)
- [En sammenligning av ulike tilfeldige tallgenereringsmetoder](https://www.random.org/randomness/)