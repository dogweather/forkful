---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Lavere case konvertering av en streng er når vi endrer alle tegnene i en tekststreng til små bokstaver. Programmerere gjør dette for å standardisere data, eliminere potensiell forvirring og forenkle sammenligningen av strenger.

## Hvordan til:
For å konvertere en streng til lavere case i Arduino, bruker vi toLowerCase() funksjonen. Her er et eksempel:

```Arduino
String melding = "Hei Verden!";
melding.toLowerCase();
Serial.println(melding); // utskriften vil være "hei verden!"
```

I dette eksempelet endrer toLowerCase() funksjonen bokstavene i "melding" variabelen til små bokstaver.

## Dyp Dykk
Historisk sett, strengmanipuleringen som konvertering til lavere case, har vært avgjørende for mange programmeringsoppgaver, spesielt for tekst analyse og databehandling. 

Et alternativ til toLowerCase() i Arduino ville være å skrive din egen funksjon for å håndtere konverteringen. Men dette er mer komplisert og utsatt for feil, så det anbefales å bruke innebygde funksjoner som toLowerCase().

Implementering av konvertering til lavere case i Arduino Zone klasse er relativt enkelt. Funksjonen går gjennom hver karakter i strengen, og hvis den finner en stor bokstav, endrer den den til en liten bokstav.

## Se Også
For mer informasjon om strengmanipulering i Arduino, sjekk ut disse ressursene:

1. [Arduino String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
2. [Text String Manipulation In Arduino](http://playground.arduino.cc/Main/TextString)
3. [Arduino Programming Notebook](http://www.arduino.cc/playground/uploads/Main/arduino_notebook_v1-1.pdf)