---
title:                "Kapitalisering av en streng"
html_title:           "Arduino: Kapitalisering av en streng"
simple_title:         "Kapitalisering av en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive en tekststreng med store bokstaver betyr å gjøre alle bokstavene i en tekststreng til blokkbokstaver. Programmerere gjør dette for å standardisere tekstdataene, noe som forenkler sammenligninger og søk.

## Hvordan:

Følgende viser hvordan du kan kapitalisere en streng i Arduino.

```Arduino
void setup() {
  Serial.begin(9600);
  
  char myString[] = "heisann verden!";
  for (int i = 0; myString[i]; i++){
    myString[i] = toupper(myString[i]);
  }
  
  Serial.print("Kapitaliserte streng: ");
  Serial.println(myString);
}

void loop() {
  // put your main code here, to run repeatedly:
}
```
Når du kjører dette eksemplet, vil output være:

```
Kapitaliserte streng: HEISANN VERDEN!
```

## Dyp Dykk:

I historikken til programmering, har behovet for å standardisere tekstdata vært nøkkelen til mye funksjoner i datatjenester. Det å kapitalisere en streng, er bare ett av mange verktøy som hjelper oss å oppnå dette.

En alternativ måte å kapitalisere en streng på, er å bruke funksjonen `strtransform()` i kombinasjon med `::toupper` i `ArduinoSTL` biblioteket. Men denne funksjonen er ikke alltid tilgengelig på alle plattformer, og kan føre til større bruk av minne.

I praksis forvandles hver bokstav i strengen til en stor bokstav ved hjelp av ASCII-verdiene. ASCII-verdien til hver lille bokstav er redusert med 32 for å få store bokstaver.

## Se Også:

For å utvide kunnskapen din, sjekk ut følgende:

1. [Arduino Reference Page on ASCII Characters](https://www.arduino.cc/en/Reference/ASCIIchart)

2. [Arduino Language Reference Page](https://www.arduino.cc/reference/en/)