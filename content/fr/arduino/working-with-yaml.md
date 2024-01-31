---
title:                "Manipulation des fichiers YAML"
date:                  2024-01-19
html_title:           "Arduino: Manipulation des fichiers YAML"
simple_title:         "Manipulation des fichiers YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Le YAML, format de sérialisation de données, est simple et lisible par l'humain. Les programmeurs utilisent YAML pour configurer des applications ou des dispositifs, notamment avec Arduino pour définir des paramètres de manière claire.

## Comment faire :

```Arduino
#include <ArduinoYAML.h>

void setup() {
  Serial.begin(9600);
  const char* yaml = 
  "title: Exemple YAML\n"
  "version: 1.0\n"
  "logging: true\n";
  
  YAML::Node config = YAML::Load(yaml);
  Serial.println(config["title"].as<String>());
  Serial.println(config["version"].as<float>());
  Serial.println(config["logging"].as<bool>());
}

void loop() {
  // Pas de contenu ici pour l'exemple
}
```

Résultat sur le moniteur série :
```
Exemple YAML
1.0
1
```

## Exploration :

Le YAML (YAML Ain't Markup Language) a été créé au début des années 2000 comme alternative à XML. Pour Arduino, on peut opter pour le JSON, mais YAML gagne pour sa facilité de lecture. L'implémentation YAML avec Arduino requiert des bibliothèques tierces comme `ArduinoYAML`, disponible via le gestionnaire de bibliothèques de l'IDE Arduino. Attention à la mémoire : les fichiers YAML complexes peuvent être gourmands en ressources.

## Voir aussi :

- Documentation officielle YAML : https://yaml.org
- ArduinoYAML sur GitHub : https://github.com/jimmiebergmann/ArduinoYAML
- Comparaison YAML vs JSON : https://json2yaml.com/
