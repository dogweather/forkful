---
title:    "Arduino: Sammenstilling av strenger"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konkatener strenger (å sette sammen to eller flere strenger til en lengre streng) i Arduino-programmering kan være nyttig når du trenger å lage en mer kompleks tekst eller melding. Det kan også gjøre koden mer effektiv og enklere å lese og forstå.

## Hvordan

For å konkatener strenger i Arduino, bruker du "+" -operatøren for å kombinere to strenger. Du kan også bruke flere "+" -operatører for å kombinere flere strenger.

```Arduino
String navn = "Ole";
String alder = "25";
String beskjed = "Hei, mitt navn er " + navn + " og jeg er " + alder + " år gammel.";
Serial.println(beskjed);
```

Dette vil gi følgende output: "Hei, mitt navn er Ole og jeg er 25 år gammel."

## Dypdykk

Når du bruker "+" -operatøren for å konkatenerere strenger, må du være oppmerksom på datatype-konvertering. Hvis du bruker forskjellige datatyper i strengene, kan det føre til uventede resultater.

Du kan også bruke funksjonen `concat()` for å konkatenerere strenger. Denne funksjonen gjør det mulig å konkatenerere flere strenger samtidig.

```Arduino
String instruksjon = "Har du " + XXX + "Når du bruker " + YYY + "merk at dette kan føre til uønskede resultater.";
Serial.println(instruksjon.concat("spørsmål om dette."));
```

Dette vil gi følgende output: "Har du spørsmål om dette? Når du bruker merk at dette kan føre til uønskede resultater."

## Se også

- [Official Arduino Language Reference - Strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [How to Concatenate Strings in Arduino](https://maker.pro/arduino/projects/concatenate-strings-arduino)