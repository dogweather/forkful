---
title:    "Arduino: Skriving til standardfeil"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error (stderr) kan være en nyttig måte å feilsøke og finne feil i din Arduino-kode. Ved å skrive til stderr kan du få mer detaljert informasjon om hva som skjer i programmet ditt og identifisere potensielle problemområder.

## Slik gjør du det

For å skrive til stderr i Arduino, kan du bruke funksjonen "Serial.print()". Denne funksjonen sender data til Arduinos serielle monitor, men du kan også sende data til stderr ved å bruke kommandoen "Serial.print()".

```Arduino
// Eksempel på bruk av Serial.print til stderr
Serial.print("Feil oppstod: ");
Serial.println(error_code, HEX); // Skriv ut feilkoden i heksadesimalt format
```

Når du kjører dette programmet, vil du kunne se feilkoden som er generert i serial monitor, slik at du kan identifisere årsaken til feilen.

## Dypdykk

Det er viktig å merke seg at writing to stderr ikke vil stoppe programmet ditt, det vil bare gi deg informasjon om potensielle feil. I tillegg, hvis stderr-utgangen ikke blir lest i et annet program eller terminal, vil den bli lagret i en buffer og kan føre til forsinkelser i utførelsen av programmet ditt. Derfor er det viktig å ikke bruke stderr i kritiske deler av koden din.

En annen ting å huske på er at stderr har begrenset plass. Hvis du skriver for mye informasjon til stderr, kan det føre til at bufferen oversvømmes og programmet krasjer. Det er derfor viktig å være forsiktig med hvor mye informasjon du sender til stderr.

## Se også

- [Arduino Serial library](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [C++ documentation on stderr](https://www.cplusplus.com/reference/cstdio/stderr/)