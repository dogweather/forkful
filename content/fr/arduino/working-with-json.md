---
title:                "Manipulation de JSON"
date:                  2024-01-19
simple_title:         "Manipulation de JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Le JSON, c'est comme le pain de mie des données : simple, léger, et pratique pour stocker des infos en paires clé-valeur. Les programmeurs l'adorent pour la communication entre matériels différents et le web.

## How to:

Pour utiliser JSON avec Arduino, faut d'abord inclure la bibliothèque `ArduinoJson`. Voici comment lire et écrire du JSON :

```arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Création d'un objet JSON
  StaticJsonDocument<200> doc;
  doc["temperature"] = 25.3;
  doc["humidite"] = 900;

  // Serialisation (Transformation en String JSON)
  serializeJson(doc, Serial);

  // Pour déserialiser, utiliser deserializeJson()
  // Exemple avec un JSON entrant : {"temperature":25.3,"humidite":900}
  StaticJsonDocument<200> incomingDoc;
  deserializeJson(incomingDoc, Serial);
  float temperature = incomingDoc["temperature"];
  int humidite = incomingDoc["humidite"];
}

void loop() {
  // Rien ici pour l'instant
}
```

Sortie série attendue après exécution :

```
{"temperature":25.3,"humidite":900}
```

## Deep Dive

Inventé par Douglas Crockford, JSON est un format parfait pour les IoT et microcontrolleurs comme Arduino. C'est une alternative tant au XML qu'aux protocoles binaires, sachant qu'il est plus facile à lire et parse. ArduinoJson est super efficace même avec peu de RAM.

## See Also

Pour en savoir plus et approfondir vos connaissances :

- ArduinoJson Lib Official Site: [arduinojson.org](https://arduinojson.org/)
- JSON Spec: [json.org](https://www.json.org/json-fr.html)
- GitHub ArduinoJson: [github.com/bblanchon/ArduinoJson](https://github.com/bblanchon/ArduinoJson)
