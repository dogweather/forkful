---
title:                "Arduino: Vérifier si un répertoire existe"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Nous entendons souvent parler de la vérification de l'existence d'un répertoire lorsqu'il s'agit de programmation, mais qu'est-ce que cela signifie réellement ? En bref, cela permet de s'assurer que le répertoire dans lequel nous voulons sauvegarder ou modifier des fichiers existe avant de commencer à travailler avec lui. Dans cet article, nous allons plonger plus en profondeur dans ce processus et découvrir pourquoi il est important de le faire dans nos projets Arduino.

## Comment faire

La vérification de l'existence d'un répertoire est un processus relativement simple à mettre en place grâce à la fonction `exists()` de la bibliothèque SD de l'Arduino. Pour l'utiliser, nous devons tout d'abord inclure cette bibliothèque dans notre code en ajoutant `#include <SD.h>` au début de notre esquisse. Ensuite, nous pouvons utiliser la fonction `exists()` en passant en paramètre le chemin vers le répertoire que nous voulons vérifier. Voici un exemple de code pour vérifier si un répertoire nommé "datalog" existe :

```
#include <SD.h>

void setup() {
  // initialisation de la carte SD
  if(!SD.begin()) {
    Serial.println("Erreur de l'initialisation de la carte SD");
    return;
  }
  
  // vérification de l'existence du répertoire "datalog"
  if (SD.exists("/datalog")) {
    Serial.println("Le répertoire existe !");
  } else {
    Serial.println("Le répertoire n'existe pas !");
  }
}

void loop() {
  // code du programme principal
}
```

Si le répertoire existe, le message "Le répertoire existe !" sera affiché dans le moniteur série. Si ce n'est pas le cas, alors le message "Le répertoire n'existe pas !" sera affiché.

## Plongée en profondeur

La fonction `exists()` renvoie une valeur booléenne (vrai ou faux) en fonction de l'existence du répertoire spécifié. Cela signifie que nous pouvons également l'utiliser en tant que condition dans une structure `if` ou `while`, par exemple :

```
// vérification de l'existence du répertoire "data" tant qu'il n'existe pas
while (!SD.exists("/data")) {
  // code pour créer le répertoire "data"
}
```

De plus, la fonction `exists()` peut également être utilisée pour vérifier l'existence de fichiers. Dans ce cas, nous passons en paramètre le chemin vers le fichier, par exemple `SD.exists("/data/test.txt")`.

## Voir aussi

Pour en savoir plus sur la bibliothèque SD de l'Arduino, vous pouvez consulter les liens suivants :
- [Documentation officielle de la bibliothèque SD](https://www.arduino.cc/en/Reference/SD)
- [Tutoriel sur l'utilisation de la carte SD avec l'Arduino](https://www.circuitbasics.com/how-to-set-up-an-sd-card-for-use-with-the-arduino/)