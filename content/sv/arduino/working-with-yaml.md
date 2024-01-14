---
title:                "Arduino: Arbeta med YAML"
simple_title:         "Arbeta med YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför
Arduino programmering är ett roligt och utmanande sätt att utforska den digitala världen. Genom att lära sig att arbeta med YAML (Yaml Ain't Markup Language) kan du utöka dina programmeringsförmågor och skapa mer komplexa och intressanta projekt med Arduino.

## Hur man gör
För att börja arbeta med YAML, behöver du installera biblioteket "ArduinoJson" i din Arduino IDE. Sedan kan du använda följande kod för att skapa ett JSON-objekt med YAML-syntax:

```Arduino
#include <ArduinoJson.h>

StaticJsonDocument<200> doc;

doc["namn"] = "Arduino";
doc["verktyg"] = "YAML";
doc["plats"] = "Sweden";
```

Detta kommer att skapa ett objekt med tre nycklar: namn, verktyg och plats. Nu kan du skriva ut detta JSON-objekt genom att använda följande kod:

```Arduino
serializeJsonPretty(doc, Serial);
```

Detta kommer att ge följande utmatning på serieporten:

```Arduino
{
    "namn": "Arduino",
    "verktyg": "YAML",
    "plats": "Sweden"
}
```

## Djupdykning
YAML är en utmärkt syntax för att strukturera data på ett lättläst sätt. Genom att använda YAML-notation kan du enkelt skapa JSON-objekt som kan användas för att lagra eller skicka data i dina Arduino-projekt. Du kan också använda YAML för att skapa dynamiska och modulära kodstrukturer, vilket gör det enkelt att bygga vidare på dina projekt och göra ändringar på ett ställe istället för att behöva ändra koden på flera platser.

## Se även
- [ArduinoJson biblioteket](https://arduinojson.org/)
- [YAML officiella hemsida](https://yaml.org/)
- [YAML-introduktion på svenska](https://www.skuggadis.se/blogg/2011/08/en-introduktion-till-yaml/)