---
title:                "Lavorare con YAML"
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Il lavoro con YAML si riferisce alla lettura, scrittura e modifica di file YAML, un formato di serializzazione user-friendly per configurazioni o dati. I programmatori utilizzano YAML per la sua leggibilità e semplicità, rendendolo ideale per i file di configurazione e lo scambio di dati tra lingue diverse.

## How to:
Arduino non gestisce direttamente YAML, ma puoi integrare librerie per fare il parsing dei dati. Ecco un esempio:

```Arduino
#include <ArduinoJson.h>

const char* yaml = 
  "- Just: Another YAML Test\n"
  "- Arduino: YAML Parser\n"
  "- Example: 3";

void setup() {
  Serial.begin(9600);
  
  DynamicJsonDocument doc(1024);
  DeserializationError error = deserializeJson(doc, yaml);

  if (error) {
    Serial.print("deserializeJson() failed: ");
    Serial.println(error.c_str());
    return;
  }

  JsonArray array = doc.as<JsonArray>();

  for (JsonObject obj : array) {
    for (JsonPair p : obj) {
      Serial.print(p.key().c_str());
      Serial.print(": ");
      Serial.println(p.value().as<const char*>());
    }
  }
}

void loop() {}
```

Output:
```
Just: Another YAML Test
Arduino: YAML Parser
Example: 3
```

## Deep Dive
YAML (YAML Ain't Markup Language), formato apparsi nei primi anni 2000, è un'alternativa a XML e JSON per configurazioni o come linguaggio intermedio. In Arduino, l'elaborazione YAML non è nativa e richiede l'intervento di librerie esterne, come ArduinoJson. Per grandi dataset, considera implementazioni più leggere del parsing per non esaurire la memoria limitata della scheda.

## See Also
- [ArduinoJson Library](https://arduinojson.org/)
- [YAML Official Website](https://yaml.org/)
