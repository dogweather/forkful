---
title:    "Fish Shell: Skrive til standardfeil"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error kan være en nyttig måte å håndtere feil og output i Fish Shell programmering. Det lar deg få en bedre forståelse av hva som skjer i programmene dine og kan hjelpe deg med å finne og løse feil raskere.

## Hvordan å

For å skrive til standard error i Fish Shell, kan du bruke kommandoen `stderr` som fungerer som en forkortelse for `standard error`. Du kan også bruke den reserverte variabelen `$stderr` for å få tilgang til standard error strømmen. Dette kan være spesielt nyttig når du feilsøker og ønsker å skrive ut feilmeldinger.

```Fish Shell
echo "Dette er en feilmelding" >&2
```

Dette eksempelet vil skrive ut teksten "Dette er en feilmelding" til standard error strømmen. Merk at `&2` sender utdataen til standard error strømmen i stedet for standard output strømmen.

En annen anvendelse av å skrive til standard error er når du bruker kommandoer som ikke genererer noen output. For eksempel, hvis du bruker kommandoen `grep` for å søke etter et mønster i en fil og det ikke finnes noen treff, vil du kanskje skrive en feilmelding til standard error for å informere brukeren om at ingenting ble funnet.

```Fish Shell
grep "mønster" fil.txt || echo "Ingen treff funnet" >&2
```

Dette sørger for at feilmeldingen blir skrevet til standard error kun hvis kommandoen "grep" returnerer en exit-kode som indikerer at ingen treff ble funnet.

## Dypdykk

Å skrive til standard error er en viktig del av feilsøking og debugging i Fish Shell programmering. Det er også nyttig når du vil skrive ut feilmeldinger eller annen informasjon til brukeren uten å forstyrre standard output strømmen. Det kan også hjelpe deg med å separere output fra ulike deler av programmet ditt, noe som kan være nyttig når du jobber med større prosjekter.

Det er også verdt å merke seg at standard error kan videresendes til en fil ved å bruke ">" operatøren. Dette kan være nyttig hvis du vil ha en permanent lagring av feilmeldinger og output fra programmene dine.

## Se også

- [Fish Shell dokumentasjon om stderr](https://fishshell.com/docs/current/cmds/stderr.html)
- [En guide til å feilsøke i Fish Shell programmering](https://coderwall.com/p/zcffaq/debugging-in-fish-shell)