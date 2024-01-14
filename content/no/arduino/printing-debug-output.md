---
title:    "Arduino: Utskrift av feilsøkingsresultater"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor

Å skrive koden din for Arduino kan være en givende og spennende opplevelse, men det kan også være utfordrende når det kommer til å finne og fikse feil. En måte å gjøre denne prosessen enklere er å skrive ut debug-informasjon under utviklingen. Dette gir deg muligheten til å se nøyaktig hva som skjer i programmet ditt, og hvor problemet ligger.

# Hvordan gjøre det

For å skrive ut debug-informasjon i Arduino, bruker du funksjonen "Serial.print()". Dette vil skrive ut informasjonen du angir i parentesene, til det serielle monitoren i Arduino-programvaren. La oss si at du har en variabel kalt "sensorverdi" som inneholder en verdi du vil skrive ut. Koden din kan da se slik ut:

```arduino
int sensorverdi = 10;
Serial.print("Sensoren leser: ");
Serial.println(sensorverdi);
```

Når du nå åpner den serielle monitoren i Arduino-programvaren, vil du se følgende utskrift:

```
Sensoren leser: 10
```

Dette gjør at du enkelt kan se verdien til variabelen din, og om den endrer seg som forventet.

# Dykk dypere

I noen tilfeller kan problemet ditt være mer komplekst enn å bare se på verdien til en variabel. I slike tilfeller kan du bruke flere Serial-kommandoer for å få mer detaljert informasjon. For eksempel kan du bruke "Serial.write()" for å skrive ut bokstavene i en tekststreng individuelt, eller "Serial.print()" for å skrive ut binære tall. Du kan også bruke "Serial.begin()" for å bestemme hastigheten på seriell kommunikasjon.

Det er også viktig å huske på at å skrive ut for mye informasjon kan føre til at Arduino henger seg opp. Derfor kan det være lurt å begrense debug-utskriftene dine eller å deaktivere dem når du er ferdig med utviklingen.

# Se også

- [Arduino Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino Serial.begin()](https://www.arduino.cc/reference/en/language/functions/communication/serial/begin/)
- [Serial Communication in Arduino](https://www.arduino.cc/en/Tutorial/Serial)
- [Arduino Troubleshooting](https://www.arduino.cc/en/Guide/Troubleshooting)