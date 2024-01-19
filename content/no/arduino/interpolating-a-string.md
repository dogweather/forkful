---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Interpolering av stringer betyr å integrere verdiene inn i en streng. Dette er nyttig for programmører fordi det tillater dem å konsolidere og forenkle uttrykk ved å blande variables inn i strenger direkte.

## Hvordan gjøre det:

Her viser vi deg hvordan interpolere en streng i Arduino med `printf` funksjonen. Se kodestumpen og utdataen nedenfor:

```Arduino
char buffer[50];
int var = 123;
printf(buffer, "Variabel verdi er: %d", var);
Serial.println(buffer);
```

Output på seriell terminal:

```
Variabel verdi er: 123
```

## Dypere dykk

Interpolering av strenger har blitt brukt i programmeringsspråk i mange år, vanligvis med bruk av prosenttegn (`%`).
I Arduino, kommer alternativer til `printf`, som kan inkludere bruk av `String`-klassen sine `concat` og `+` operatør. Bruk av interpolering kan variere ut ifra versjonen av Arduino.

```Arduino
// Alternativ måte å gjøre strenginterpolering
char variabel[10] = "123"; 
String streng = "Variabel verdi er: "; 
streng += variabel; 
Serial.println(streng);
```

## Se også

- For mer informasjon om printf på Arduino, referer til [printf Arduino](https://www.arduino.cc/reference/en/)
- For alternativer til interpolering av strenger i Arduino, se [Interpolating Strings in Arduino](https://arduinogetstarted.com/tutorials/arduino-string-format)
  
Husk, effektiv programmering handler om å kjenne dine verktøy. Se referansene for å ytterligere forstå interpolering av strenger!