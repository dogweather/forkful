---
title:    "Arduino: Stor bokstav på en streng"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor?

Det å konvertere en streng til store bokstaver kan være et viktig element i programmering, spesielt når man jobber med brukerinput eller håndterer tekstbaserte data. Det kan bidra til å strømlinjeforme prosessen og sikre en konsistent formatering av tekst.

## Hvordan gjøre det

For å konvertere en streng til store bokstaver i Arduino, kan du bruke funksjonen `toUpperCase()` som tar inn en streng som parameter. Her er et eksempel som tar inn et navn og konverterer det til store bokstaver:

```Arduino
String navn = "Johannes";
String konvertert = navn.toUpperCase();
Serial.println(konvertert);
```

Dette vil gi følgende utskrift i seriell monitoren:

```Arduino
JOHANNES
```

Hvis du ønsker å konvertere en streng i et array eller en liste med tegn, kan du bruke en `for`-løkke for å iterere gjennom hvert tegn og bruke funksjonen `toupper()` for å konvertere dem til store bokstaver. Se et eksempel nedenfor:

```Arduino
char navn[] = "Johannes";
for (int i = 0; i < strlen(navn); i++) {
    navn[i] = toupper(navn[i]);
}
Serial.println(navn);
```

Dette vil også gi utskrift av navnet i store bokstaver.

## Dypdykk

Det finnes også andre måter å konvertere en streng til store bokstaver på i Arduino, som for eksempel å bruke funksjonen `charAt()` for å hente ut hvert tegn og gjøre en sjekk på om det er et lite tegn ved å bruke `isLower()`. Dette gir mulighet for å skrive en egen funksjon som konverterer en streng til store bokstaver.

Det er også viktig å være oppmerksom på forskjellige tegnsett, da noen bokstaver kan ha forskjellig verdier avhengig av språk. Det kan være lurt å bruke funksjonen `setCharSet()` for å sikre at de riktige verdiene blir konvertert til store bokstaver.

## Se også

- [String-funksjoner i Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [ASCII-tabellen](https://ascii.cl/)