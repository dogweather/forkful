---
title:                "Arduino: Skriver til standardfeil"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan ofte være nødvendig å skrive informasjon til standard error i Arduino-programmering. Dette kan være nyttig for debugging og feilsøking, da utskrift til standard error vil vises i Serial Monitor og gi deg bedre innsikt i hva som skjer i koden din.

## Hvordan gjøre det

For å skrive til standard error i Arduino, bruker du funksjonen ```Serial.print()``` eller ```Serial.println()```, og setter deretter parameteren til ```stderr```. Her er et eksempel på hvordan dette kan se ut i koden din:

```Arduino
Serial.println("Dette er en feilmelding", stderr);
```

Når du laster opp koden og åpner Serial Monitor, vil du se at meldingen vises med røde bokstaver for å indikere at det er en feilmelding.

## Dypdykk 

Ved å skrive til standard error i Arduino, kan du få mer informasjon om potensielle feil i koden din og pinpointe hvor problemet ligger. Dette kan være spesielt nyttig når du jobber med større eller komplekse koder, da det kan være vanskelig å finne feilene kun ved å se på koden.

## Se også

- [Arduino Serial.print() referanse](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino Serial.println() referanse](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Feilsøking i Arduino: Slik bruker du Serial Monitor](https://www.roboticsclassesinpune.com/blogs/arduino-debugging-serial-monitor/)