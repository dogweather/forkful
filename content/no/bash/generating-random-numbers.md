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

## Hva og hvorfor?
Generering av tilfeldige tall er en vanlig oppgave for programmerere. Dette betyr rett og slett å lage et tall uten noen spesiell sammenheng eller mønster. Dette kan være nyttig for å skape variasjon i data, teste kodesnutter eller for spill og simuleringer.

## Slik gjør du:
For å generere et tilfeldig tall i Bash, kan du bruke kommandoen ```$RANDOM```. Dette vil gi deg et tall mellom 0 og 32767. For eksempel:

```Bash
echo $RANDOM 
# Output: 24988
```
Hvis du ønsker et annet tall mellom en spesifikk rekkevidde, for eksempel mellom 1 og 100, kan du bruke følgende kode:

```Bash
echo $((RANDOM % 100 + 1))
# Output: 56 (for eksempel)
```

## Dypdykk:
Generering av tilfeldige tall har vært et viktig konsept innenfor matematikk og informatikk i lang tid. En av de første metodene for å generere tilfeldige tall ble utviklet av den kjente matematikeren John von Neumann på 1940-tallet. I dag finnes det mange algoritmer og teknikker for å generere tilfeldige tall, og noen programmeringsspråk har dedikerte funksjoner for dette.

Alternativer til å bruke ```$RANDOM``` i Bash inkluderer å bruke andre programmeringsspråk som har god støtte for tilfeldige tall, som for eksempel Python eller Java. Du kan også bruke eksterne verktøy som ```shuf``` for å sortere tilfeldige tall eller ```openssl``` for å generere kryptografisk sikre tilfeldige tall.

Generering av tilfeldige tall i Bash baserer seg på en algoritme som bruker tid og prosessnumre for å skape variasjon. Dette betyr at tallene ikke er helt tilfeldige, men bare tilfeldige nok for de fleste bruksområder. For mer sensitive applikasjoner bør du bruke kryptografiske metoder for å sikre tilfeldighet.

## Se også:
[How to generate random numbers in Bash](https://opensource.com/article/19/2/random-numbers-bash)