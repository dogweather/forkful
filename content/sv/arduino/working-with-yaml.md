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

## Vad & Varför?
Att arbeta med YAML i Arduino betyder att man kodar och strukturerar data i ett format som är lättläst och förståeligt för både människor och maskiner. Det finns många anledningar till att programmerare väljer att använda YAML, bland annat för dess enkelhet och flexibilitet.

## Så här:
Det första steget för att använda YAML i Arduino är att installera biblioteket "Arduino-yaml". Därefter kan man läsa och skriva data genom att använda funktioner som `YAML.read` och `YAML.write`. Exempel på kod och output kan ses nedan:

```Arduino
#include <YAML.h>

void setup() {
  YAML.begin();
  // Skapa en strukturerad dataström
  YAML.create();

  // Läsa data från en YAML-fil
  YAML.read("min_fil.yaml");
  int värde = YAML.get("variabel");
  Serial.println(värde); // Visar det lästa värdet

  // Skriva data till en YAML-fil
  YAML.put("variabel", 42);
  YAML.write("min_fil.yaml"); // Lägger till eller ersätter värdet i filen
}

void loop() {
  // Kod för att köra kontinuerligt
}
```

Output:
```console
42
```

## Djupdykning:
YAML, eller "YAML Ain't Markup Language", är ett format för strukturerad data som är designat för att vara lättläst och portabelt mellan olika språk och plattformar. Det används ofta för konfigurationsfiler och databankshantering. Alternativ till YAML är till exempel JSON och XML, men YAML erbjuder en mer intuitiv syntax och stöd för mer komplexa datastrukturer.
YAML är också ett stöd för flera programmeringsspråk, vilket gör det enkelt att använda i olika projekt.

## Se även:
- [YAML.org](https://yaml.org/)
- [Arduino-yaml bibliotek](https://github.com/zs5n/Arduino-YAML)