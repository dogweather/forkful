---
title:                "Arduino: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Arduino, vous savez probablement déjà combien il est important d'organiser efficacement vos fichiers et répertoires. Mais que se passe-t-il si vous souhaitez vérifier si un répertoire existe avant de le créer ou d'y enregistrer des fichiers ? Dans cet article, nous allons vous montrer comment faire cela en utilisant Arduino.

## Comment faire 

```Arduino
void setup() {
  Serial.begin(9600); //Initialiser la communication série à 9600 bauds
  if (SD.exists("/mon_repertoire")) { //Vérifier si le répertoire existe
    Serial.println("Le répertoire existe !");
  } else {
    Serial.println("Le répertoire n'existe pas !");
  }
}

void loop() {
  
}
```
Lorsque vous allez exécuter le code ci-dessus, il vérifiera si le répertoire "mon_repertoire" existe sur votre carte SD. Si c'est le cas, il affichera "Le répertoire existe !" sur le moniteur série. Sinon, il affichera "Le répertoire n'existe pas !".

## Plongée en profondeur

La fonction `SD.exists()` que nous avons utilisée dans l'exemple ci-dessus est une fonction définie dans la bibliothèque `SD.h`. Cette fonction prend le chemin absolu du répertoire en paramètre et renvoie `true` s'il existe, ou `false` s'il n'existe pas.

Il est également important de noter que cette fonction n'est compatible qu'avec les cartes SD utilisant le système de fichiers FAT16 ou FAT32. Si vous utilisez une carte SD avec un système de fichiers différent, cette fonction ne fonctionnera pas.

## Voir aussi

- [Tutoriel vidéo sur l'utilisation de la carte SD avec Arduino](https://www.youtube.com/watch?v=jiR4XFByBU4)
- [Documentation officielle sur la bibliothèque SD](https://www.arduino.cc/en/Reference/SD)
- [Forum Arduino pour poser des questions et obtenir de l'aide](https://forum.arduino.cc/index.php?board=4.0)