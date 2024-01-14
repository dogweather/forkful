---
title:                "Arduino: Utskrift av feilsøkingsutdata"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å printe debug-utdata er en viktig del av å programmere Arduino. Det hjelper deg med å finne og feilsøke problemer i koden din.

## Slik gjør du det

For å printe debug-utdata i Arduino, kan du bruke funksjonen "Serial.print()". Du kan også bruke "Serial.println()" for å skrive ut en linje med tekst og gå til neste linje. Her er et eksempel på hvordan du kan bruke disse funksjonene:

```Arduino
int verdi = 42; // Opprett en variabel og gi den verdien 42

Serial.print("Denne variabelen har verdien: "); // Skriv ut en tekstlinje
Serial.println(verdi); // Skriv ut verdien av variabelen og gå til neste linje
```

Output vil se slik ut i Serial Monitor:

```
Denne variabelen har verdien: 42
```

## Dykker dypere

Det finnes flere måter å bruke "Serial.print()" og "Serial.println()" på, som å skrive ut tekst, tall, og til og med verdien av sensorer som er koblet til Arduino. Du kan også bruke "Serial.begin()" for å initialisere Serial kommunikasjonen og sette et baudrate (datahastighet) for å sikre at utdata blir sendt og mottatt riktig.

En annen nyttig funksjon i debugging er "Serial.available()". Denne funksjonen lar deg sjekke om det er data tilgjengelig til å bli lest fra Serial monitor. Det er spesielt nyttig når du arbeider med seriell kommunikasjon mellom Arduino og en datamaskin.

## Se også

- [Official Arduino Serial reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino - Debugging](https://www.arduino.cc/en/Guide/Troubleshooting#debugging)
- [Arduino Serial Monitor: Ultimate Guide](https://randomnerdtutorials.com/arduino-serial-monitor-ultimate-guide/)