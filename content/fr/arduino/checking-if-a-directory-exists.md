---
title:                "Vérifier si un répertoire existe"
html_title:           "Arduino: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Vérifier si un répertoire existe est une fonctionnalité importante pour les programmeurs travaillant avec des systèmes de fichiers. Cela permet de s'assurer qu'un répertoire nécessaire pour le bon fonctionnement d'un programme est bien présent sur l'appareil.

## Comment faire:
Utiliser la fonction ```ArduinoFile.exists()``` permet de vérifier simplement si un répertoire existe ou non. Si le répertoire existe, cette fonction renverra "true" et si ce n'est pas le cas, elle renverra "false". Voici un exemple de code qui utilise cette fonction:

```Arduino
if (ArduinoFile.exists("/monRepertoire")) {
  Serial.println("Le répertoire existe !");
} else {
  Serial.println("Le répertoire n'existe pas !");
}
```

Lorsque ce code est exécuté, le moniteur série affichera "Le répertoire existe !" si le répertoire "/monRepertoire" existe, sinon il affichera "Le répertoire n'existe pas !".

## Plongée en profondeur:
La possibilité de vérifier si un répertoire existe est venue avec l'ajout des systèmes de fichiers dans les versions récentes d'Arduino. Avant cela, les programmeurs devaient utiliser des bibliothèques tierces pour accéder et gérer les fichiers sur leurs appareils. Il existe également d'autres façons de vérifier si un répertoire existe, comme utiliser la commande système "ls".

## Voir aussi:
Vous pouvez trouver plus d'informations sur la fonction ```ArduinoFile.exists()``` sur la [documentation officielle d'Arduino](https://www.arduino.cc/en/Reference/ArduinoFilesExists). Vous pouvez également consulter des tutoriels en ligne pour en apprendre davantage sur la gestion des fichiers avec Arduino.