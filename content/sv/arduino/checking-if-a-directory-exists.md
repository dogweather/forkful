---
title:    "Arduino: Kontrollera om en mapp finns"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar kan vara en viktig del av att skapa en välstrukturerad och effektiv kod för ditt Arduino-projekt. Genom att inkludera denna funktion i ditt program kan du säkerställa att din kod fungerar smidigt och undvika onödiga felmeddelanden.

## Så här gör du

För att kontrollera om en mapp existerar kan du använda funktionen `exists()` i Arduinos `File` bibliotek. Detta bibliotek ger dig möjlighet att utföra filåtgärder med hjälp av en `File`-objekt. Här är ett enkelt exempel på hur du kan använda `exists()` för att kontrollera om en mapp med namnet "sensorData" existerar:

```
Arduino File sensorData;
if (sensorData.exists()) {
    Serial.println("Sensor mappen finns!");
} else {
    Serial.println("Sensor mappen finns inte!");
}
```

Om mappen finns kommer du se meddelandet "Sensor mappen finns!" i seriell monitor. Om mappen inte finns kommer du få meddelandet "Sensor mappen finns inte!".

## Djupdykning

För att använda `exists()` funktionen behöver du först skapa ett `File`-objekt för din mapp. Det är också viktigt att notera att om du vill kontrollera om en undermapp existerar, så behöver du först skapa ett `File`-objekt för den överordnade mappen. Du kan också använda `dirName()` funktionen för att kontrollera en specifik undermapp inom en mapp. Här är ett exempel på hur du kan göra det:

```
Arduino File dataFolder = SD.open("sensorData");
if (dataFolder.exists("temperature")) {
    Serial.println("Temperaturmappen finns inuti sensor mappen!");
} else {
    Serial.println("Temperaturmappen finns inte inuti sensor mappen!");
}
```

## Se också

- `exists()` i Arduino referensdokumentationen: https://www.arduino.cc/en/Reference/EXISTS
- `File` biblioteks dokumentationen: https://www.arduino.cc/en/Reference/SD
- Mer om filåtgärder med Arduino: https://create.arduino.cc/projecthub/Arduino_Genuino/using-the-sd-card-6f42f4