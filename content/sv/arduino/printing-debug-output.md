---
title:    "Arduino: Utskrift av felsökningsutmatning"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför

Att skriva ut debuggutdata är ett användbart verktyg för att hitta och lösa problem i din Arduino-kod. Genom att skriva ut värden på olika variabler och steg i din kod kan du få en bättre förståelse för hur den fungerar och varför eventuella fel uppstår.

## Så här gör du

För att skriva ut debuggutdata i din Arduino-kod, använd funktionen Serial.println(). Detta kommer att skriva ut värdet på en variabel eller en sträng i seriell monitor. Här är ett exempel på hur du kan använda det i din kod:

```Arduino
int sensorValue = analogRead(A0);  // läs av ett värde från pin A0
Serial.println(sensorValue);  // skriv ut värdet i seriell monitor
```

När du laddar upp koden till din Arduino, öppna sedan seriell monitor (Tools > Serial Monitor) för att se utdatan. Om du vill skriva ut fler värden, använd bara flera Serial.println() funktioner i din kod. Det är också möjligt att använda Serial.print() för att skriva ut data utan radbrytning.

## Djupdykning

Att skriva ut debuggutdata kan vara särskilt användbart när du arbetar med sensorer eller andra externa enheter. Genom att skriva ut sensordata kan du se om dina mätningar är korrekta och i vilken utsträckning de påverkas av yttre faktorer.

Du kan också använda Serial.println() för att bekanta dig med dina kodblock. Genom att skriva ut värden på variabler, kan du följa hur de ändras under körning och felsöka eventuella fel.

En annan fördel med att skriva ut debuggutdata är att det kan hjälpa dig att förbättra prestandan på din kod. Genom att skriva ut tiden det tar för olika delar av koden att köras, kan du identifiera flaskhalsar och optimera din kod för att göra den snabbare och effektivare.

## Se även

- [Official Arduino Serial Documentation](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Tutorial: Serial Communication](https://www.arduino.cc/en/Tutorial/Serial)
- [Debugging Arduino Code Using Serial.print()](https://create.arduino.cc/projecthub/cesarzambrano/debugging-arduino-code-using-serial-print-944616)