---
title:                "Utskrift av felsökningsresultat"
html_title:           "Arduino: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut debug output är när man ber Arduinoen att visa information om vad som händer i koden när den körs. Programmerare använder det för att felsöka och förstå hur koden fungerar.

## Så här gör du:
```
Arduino.print("Hej värld!");
```
Koden ovan kommer att skriva ut "Hej värld!" på seriell monitor vid programkörning.

```
int a = 5;
int b = 10;
int summa = a + b;
Arduino.print(summa);
```
Koden ovan kommer att skriva ut värdet av variabeln "summa" på seriell monitor.

## Djupdykning:
Att skriva ut debug output är ett vanligt verktyg för felsökning, men det finns gott om alternativ som t.ex. att använda en debugger eller att använda någon form av loggning. Det är viktigt att inte ha för mycket debug output i koden då det kan påverka prestandan negativt. För att använda debug output på Arduino måste man ha en seriell anslutning mellan Arduinoen och datorn.

## Så här kan du använda dig av debug output:
1. Använd kommandot "Serial.print()" för att skriva ut text och variabler på seriell monitor.
2. Använd kommandot "Serial.println()" för att skriva ut en radbrytning efter varje utprintning.
3. Använd kommandot "Serial.begin()" för att öppna en seriell anslutning mellan Arduinoen och datorn.

## Se även:
- [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Using Debug Output in Arduino](https://www.arduino.cc/en/Guide/ArduinoDebugging)