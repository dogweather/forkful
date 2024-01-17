---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Arduino: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive ut debug output er en måte for programmerere å få informasjon om hvordan programmet fungerer mens det kjører. Dette gjør det enklere å finne og feilsøke eventuelle problemer i koden.

## Hvordan:

For å skrive ut debug output i Arduino, bruker vi funksjonen ```Serial.print()```. Dette lar oss skrive ut tekst eller variabler til seriell-monitoren i Arduino IDE. La oss se på et eksempel:

```
int skole_timer = 5;
Serial.print("Antall timer på skolen: ");
Serial.print(skole_timer); // Printer variabelen skole_timer
```

Dette vil skrive ut følgende i seriell-monitoren:

```
Antall timer på skolen: 5
```

## Dypdykk:

Å skrive ut debug output har vært en viktig del av programmering siden begynnelsen. Det lar oss få et bedre forståelse av hvordan koden fungerer og identifisere eventuelle problemer. Alternativet til å skrive ut debug output er å bruke en debugger, men dette kan være mer komplisert og ikke alltid tilgjengelig. I tillegg kan seriell-monitoren til Arduino brukes til å kommunisere med annet utstyr, som for eksempel sensorer eller skjermer.

For å skrive ut tall i seriell-monitoren, må vi bruke ```Serial.println()``` i stedet for ```Serial.print()```. Dette vil legge til en linjeskift etter teksten eller variabelen. Det er også mulig å skrive ut tall i binær eller hexadecimal format ved å bruke hhv. ```Serial.print(BIN)``` og ```Serial.print(HEX)```.

## Se også:

- Docs for ```Serial.print()``` i [Arduino Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- Tutorial: [Skrive ut til seriell-monitoren med Arduino](https://www.arduino.cc/en/Tutorial/SerialPrint)