---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML står för "YAML Ain't Markup Language" (rekursiv akronym) och är ett format för dataseriering som är enkelt för människor att läsa och skriva. Programmerare använder YAML för konfigurationsfiler och datautbyte eftersom det är lättläst och ofta enklare än XML eller JSON.

## How to:
Arduino har inte inbyggt stöd för YAML, men här är ett exempel på hur du kan läsa en enkel YAML-konfigurationsfil med pseudokod (tänk på att du kan behöva en extern parser):

```Arduino
// Pseudokod för att hantera YAML-data
YAMLParser parser = new YAMLParser();
YAMLData data = parser.parse(yamlConfig);
int threshold = data.getInt("threshold");
String mode = data.getString("mode");

Serial.print("Tröskelvärde: ");
Serial.println(threshold);
Serial.print("Läge: ");
Serial.println(mode);
```

Förväntad utskrift:
```
Tröskelvärde: 10
Läge: Auto
```

Notera: Arduino-kodexempel ovan antar att det finns en fiktiv biblioteksklass `YAMLParser` som kan hantera YAML-strängar.

## Deep Dive
YAML designades i början av 2000-talet för att vara mer läsbar och enkel jämfört med andra dataformat. Alternativ till YAML inkluderar JSON, som är mer kompakt och generellt snabbare att analysera, och XML, som är ambitiösare och har stöd för scheman och namnrymder. I Arduino-sammanhang används YAML sällan eftersom det saknar inbyggt stöd och enheterna oftast har begränsade resurser, så JSON eller helt enkla egna format är vanligare.

## See Also
- YAML officiell webbplats: https://yaml.org
- JSON: https://www.json.org/json-sv.html
- XML: https://www.w3.org/XML/