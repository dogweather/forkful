---
title:    "Arduino: Vérifier si un dossier existe"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez avec Arduino, il est parfois nécessaire de vérifier si un répertoire existe. Cela peut être utile pour organiser et gérer vos fichiers de manière efficace. Heureusement, il est possible de le faire facilement en utilisant quelques lignes de code.

## Comment faire

Pour vérifier si un répertoire existe dans Arduino, vous pouvez utiliser la fonction `exists()` de la bibliothèque `SD`. Cette fonction retourne une valeur booléenne (vrai ou faux) en fonction de l'existence du répertoire spécifié. Prenons un exemple concret :

```
#include <SD.h>

bool directoryExists = SD.exists("mon_repetoire"); // Vérifie si le répertoire "mon_repetoire" existe

if (directoryExists) {
  Serial.println("Le répertoire existe !");
} else {
  Serial.println("Le répertoire n'existe pas.");
}
```

Si le répertoire "mon_repetoire" existe, le message "Le répertoire existe !" sera affiché dans le moniteur série. Sinon, le message "Le répertoire n'existe pas." sera affiché.

## Plongée Approfondie

Il est important de noter que la fonction `exists()` vérifie uniquement l'existence du répertoire et non s'il est vide ou non. De plus, elle n'accepte que les noms de répertoires relatifs à la racine de la carte SD.

Si vous souhaitez vérifier si un fichier existe dans un répertoire spécifique, vous devrez d'abord naviguer vers ce répertoire en utilisant la fonction `chdir()`.

## Voir aussi

- [Documentation officielle SD.h](https://www.arduino.cc/en/Reference/SD)
- [Tutoriel Arduino: Comment utiliser une carte SD avec Arduino](https://www.arduino.cc/en/Tutorial/Files)
- [Forum Arduino: Comment vérifier l'existence d'un répertoire](https://forum.arduino.cc/t/check-if-the-folder-exists-or-not-for-sd-card-calendar-ds1307/372835)