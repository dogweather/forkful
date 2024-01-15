---
title:                "Stor bokstaver i en streng"
html_title:           "Fish Shell: Stor bokstaver i en streng"
simple_title:         "Stor bokstaver i en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen bry seg om å gjøre om en streng til stor bokstav? Vel, det kan være flere grunner. Kanskje du ønsker å formatere en utskrift for å gjøre den mer lesbar, eller kanskje du trenger å sammenligne strenger i en nøkkelverdiliste. Det kan også bare være en del av din personlige stil som kodespråkbruker. Uansett grunn, er det nyttig å kunne gjøre dette enkelt og effektivt i Fish Shell.

## Slik gjør du det

Hvis du vil gjøre om en streng til stor bokstav i Fish Shell, kan du bruke kommandoen `string upper-case`:

```
Fish Shell> string upper-case "dette er en test"
DETTE ER EN TEST
```

Du kan også bruke `string lower-case` for å gjøre strengen til små bokstaver.

```
Fish Shell> string lower-case "Dette ER En TeSt"
dette er en test
```

Hvis du vil gjøre en del av en streng til stor eller liten bokstav, kan du bruke `string sub` kommandoen. Du kan angi start- og sluttindekser for å spesifisere hvilken del av strengen du vil endre.

```
Fish Shell> string sub "Dette er en test" 0 5 (string upper-case "dette")
DETTE er en test
```

## I dybden

I Fish Shell er `string` en struktur som inneholder forskjellige nyttige kommandoer for strenger, inkludert `upper-case` og `lower-case`. `sub` kommandoen lar deg gjøre endringer på en del av en gitt streng. Du kan også bruke regulære uttrykk med disse kommandoene for å gjøre mer avanserte endringer på tekststrenger.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Minimum-viable-fish](https://github.com/jorgebucaran/minimum-viable-fish)