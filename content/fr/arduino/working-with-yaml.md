---
title:                "Travailler avec yaml"
html_title:           "Arduino: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
YAML (YAML Ain't Markup Language) est un langage de sérialisation de données qui permet aux programmeurs de structurer et de stocker des informations sous forme de texte. Les programmeurs utilisent YAML pour rendre les données lisibles et faciles à modifier, ce qui est particulièrement utile lors de la configuration de périphériques ou de la création de fichiers de configuration.

## Comment faire:
```arduino
#include <YAML.h>

void setup() {
  // initialise la communication avec le port série
  Serial.begin(9600);
  // crée un objet YAML
  YAMLClass yaml;
  // stocke un tableau de valeurs dans un bloc YAML
  const char *yamlData = "mesures:\n  - temperature: 25\n  - pression: 1013";
  // charge les données YAML
  yaml.begin(yamlData);
  // imprime la température dans le bloc YAML
  Serial.print("Température: ");
  Serial.println(yaml["mesures"][0]["temperature"].as<int>());
  // imprime la pression dans le bloc YAML
  Serial.print("Pression: ");
  Serial.println(yaml["mesures"][1]["pression"].as<int>());
}

void loop() {
  // programme principal
}
```

## Plongée profonde:
YAML a été créé en 2001 et est basé sur la syntaxe des listes dans le langage de programmation Python. Il est souvent utilisé pour remplacer d'autres formats de données tels que JSON ou XML en raison de sa simplicité et de sa facilité de lecture. Les bibliothèques de YAML sont disponibles pour la plupart des langages de programmation, y compris pour l'Arduino.

## Voir aussi:
- [Documentation officielle YAML](https://yaml.org)
- [Bibliothèque YAML pour Arduino](https://www.arduinolibraries.info/libraries/yaml)
- [Format de données YAML sur Wikipédia](https://fr.wikipedia.org/wiki/YAML)