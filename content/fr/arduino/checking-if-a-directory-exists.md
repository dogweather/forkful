---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Arduino: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous utilisez Arduino pour stocker des données dans une carte SD, il peut être utile de vérifier si le répertoire dans lequel vous souhaitez stocker ces données existe déjà. Cela permet d'éviter d'écraser involontairement des données précieuses.

## Comment faire
```Arduino
#include <SD.h>

bool directoryExists(const char* path) {
  if (SD.exists(path)) {
    return true;
  } else {
    return false;
  }
}

void setup() {
  Serial.begin(9600);
  SD.begin(10);  // Démarre la communication avec la carte SD
}

void loop() {
  if (directoryExists("/monRepertoire")) { // Vérifie si le répertoire existe
    Serial.println("Le répertoire existe!");
  } else {
    Serial.println("Le répertoire n'existe pas");
  }
}
```

Sortie: 
```
Le répertoire existe! // Si le répertoire existe
Le répertoire n'existe pas // Si le répertoire n'existe pas
```

## Plongée en profondeur
La fonction ```SD.exists(path)``` est incluse dans la bibliothèque SD pour Arduino. Elle renvoie ```true``` si le chemin spécifié existe et ```false``` sinon. Il est important de noter que cette fonction vérifie uniquement l'existence du répertoire, et non son contenu.

## Voir aussi
- [Documentation officielle de la bibliothèque SD pour Arduino](https://www.arduino.cc/en/Reference/SD)
- [Tutoriel vidéo sur l'utilisation de la carte SD avec Arduino](https://www.youtube.com/watch?v=bmu0M6ud5S8)