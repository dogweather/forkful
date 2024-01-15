---
title:                "Att arbeta med yaml"
html_title:           "Arduino: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

Vad är YAML och varför borde du använda det med Arduino?

YAML står för "YAML Ain't Markup Language" och det är ett format för att strukturera data som är lätt att läsa och skriva för människor. Det är också lätt att läsa för datorer och används ofta för konfigurationsfiler inom mjukvaruutveckling. Med YAML kan du skapa och ändra datastrukturer på ett enkelt sätt, vilket är särskilt användbart när du arbetar med Arduino.

## Varför

Det finns många anledningar till varför du skulle vilja använda YAML med Arduino. Ett av de främsta skälen är att det gör det möjligt att skriva och läsa datastrukturer på ett enkelt sätt, vilket är perfekt för att hantera konfigurationsfiler som styr din Arduino-kod.

Med YAML kan du också enkelt göra ändringar i dina datastrukturer utan att behöva ändra koden i din Arduino-sketch. Detta gör det mycket lättare att hantera och uppdatera dina projekt.

## Så här gör du

För att använda YAML inom Arduino behöver du lägga till ett bibliotek som heter "YamlDuino" i din uppsättning av Arduino-bibliotek.

```Arduino
#include <YamlDuino.h>
YAML.begin();
```

Sedan kan du skapa dina datastrukturer i YAML-format genom att använda funktionen `YAML.load()` och ge den en sträng som innehåller din YAML-kod.

```Arduino
String myData = "name: John\nage: 30";
YAMLObject dataObj = YAML.load(myData);
```

Nu kan du enkelt hämta och ändra värdena i din datastruktur med hjälp av punktnotation.

```Arduino
String name = dataObj.name;
int age = dataObj.age;

dataObj.age = 31;
```

Du kan också skapa och spara nya YAML-filer direkt på ditt Arduino-kort med hjälp av funktionen `YAML.save()`.

```Arduino
String newFile = "newData.yaml";
YAML.save(newFile, dataObj);
```

## Djupdykning

En av de största fördelarna med YAML är dess enkelhet och läsbarhet för människor. Detta gör det mycket lätt att ändra och uppdatera datastrukturer efter behov.

En annan fördel är att YAML stöder olika typer av data, inklusive strängar, heltal, flyttal och listor. Detta gör det möjligt att skapa komplexa datastrukturer som är lättare att hantera och bearbeta.

För att lära dig mer om YAML och dess olika funktioner, se till att läsa dokumentationen för YamlDuino-biblioteket samt YAMLs officiella hemsida.

## Se även

- [YamlDuino biblioteket](https://github.com/gmag11/YamlDuino)
- [YAML officiella hemsida](https://yaml.org)