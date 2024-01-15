---
title:                "Utskrift av debug output"
html_title:           "Arduino: Utskrift av debug output"
simple_title:         "Utskrift av debug output"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Debug output, eller felsökningsutskrift, är ett användbart verktyg som hjälper dig att hitta och lösa problem i din Arduino-kod. Genom att skriva ut viktiga variabler och värden i din kod kan du enkelt spåra var problemet ligger och göra nödvändiga korrigeringar.

## Så här gör du
För att skriva ut debug output i Arduino behöver du bara använda funktionen "Serial.print()". Till exempel kan du skriva:

```Arduino
int sensorValue = analogRead(A0);
Serial.print("Sensorvärde: ");
Serial.println(sensorValue);
```

Detta kommer att skriva ut värdet av sensorn till din seriella monitor. Kom ihåg att du måste öppna serial monitor genom att klicka på "Verktyg" och sedan "Seriell monitor" för att se utskriften. 

Du kan också använda "Serial.println()" för att skriva ut en tom rad efter varje utskrift för att få en mer läsbar output. Detta kan vara särskilt användbart när du skriver ut flera variabler.

## Djupdykning
I Arduino kan du också använda dig av olika formatteringsalternativ när du skriver ut debug output. Till exempel kan du använda "Serial.print()" tillsammans med "%s" för att skriva ut en textsträng:

```Arduino
String namn = "Anna";
Serial.print("Hej, mitt namn är %s!", namn.c_str());
```

Du kan också använda "%d" för att skriva ut heltal och "%.2f" för att skriva ut decimaltal med två decimaler.

Debug output är också användbart när du ska läsa av sensorer och moduler som kommunicerar över I2C eller SPI. Genom att skriva ut rådata från dessa moduler kan du se om de fungerar korrekt och enkelt felsöka eventuella problem.

## Se också
Om du vill lära dig mer om hur man använder debug output i Arduino, kan du kolla in följande resurser:

- [Arduino Serial print() - Lär dig hur man använder funktionen Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino Serial monitor - Lär dig hur man öppnar och använder den seriella monitorn för att visa debug output](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Viktigheten av debug output i Arduino - En artikel som förklarar varför debug output är en viktig del av Arduino-utveckling](https://blog.arduboy.com/the-importance-of-debug-output/)

Nu är det dags att börja utnyttja debug output i din Arduino-kod för en smidigare och mer effektiv utvecklingsprocess!