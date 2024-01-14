---
title:                "Arduino: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un amateur de programmation et que vous cherchez à améliorer vos compétences en langage Arduino, alors apprendre à travailler avec YAML pourrait être un bon défi pour vous. YAML est un format de données très utile pour stocker et transmettre des données structurées entre différents logiciels et systèmes, ce qui le rend très populaire dans le monde de la programmation. Non seulement cela, mais il est également facile à apprendre et à utiliser, en particulier lorsque vous travaillez avec Arduino.

## Comment faire

Pour travailler avec YAML dans un projet Arduino, vous devrez d'abord installer la bibliothèque YAML Arduino. Cette bibliothèque vous permettra de lire et d'écrire des données YAML dans vos programmes Arduino. Voici un exemple de code pour lire des données YAML à partir d'un fichier et les stocker dans une variable :

```
#include <YAMLCpp.h>

YAMLCpp yaml;

void setup() {
  Serial.begin(9600);

  File file = SPIFFS.open("/donnees.yaml");

  if (!file) {
    Serial.println("Erreur lors de l'ouverture du fichier.");
  }

  // Lecture du fichier YAML dans une variable
  String donnees = yaml.load(file);

  Serial.println(donnees);

  file.close();
}

void loop() {
  // Code à exécuter en boucle
}
```

Dans cet exemple, nous utilisons la bibliothèque YAML Arduino pour ouvrir le fichier "donnees.yaml" et stocker son contenu dans la variable "donnees". Vous pouvez ensuite utiliser ces données dans votre code Arduino selon vos besoins.

## Plongée en profondeur

Maintenant que vous savez comment lire des données YAML dans votre projet Arduino, vous pouvez également apprendre à écrire des données YAML. Pour cela, vous pouvez utiliser la fonction "yaml.dump()" qui vous permet de convertir vos données en format YAML et de les écrire dans un fichier. Vous pouvez également utiliser la bibliothèque pour manipuler des données plus complexes telles que des tableaux et des objets.

Comme pour toute nouvelle compétence, la pratique est la clé pour maîtriser YAML dans vos projets Arduino. Expérimentez avec différents exemples de code et consultez les ressources en ligne pour en apprendre davantage sur les fonctionnalités et les possibilités de cette bibliothèque.

## Voir aussi

- Documentation de la bibliothèque YAML Arduino : https://github.com/oxullo/Arduino-Yaml
- Tutoriel sur l'utilisation de YAML avec Arduino : https://learn.sparkfun.com/tutorials/how-to-work-with-yaml-in-the-ardeno-environment/all
- Exemples de projets utilisant YAML : https://github.com/topics/arduino-yaml