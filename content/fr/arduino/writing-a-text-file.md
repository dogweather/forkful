---
title:                "Ecrire un fichier texte"
html_title:           "Arduino: Ecrire un fichier texte"
simple_title:         "Ecrire un fichier texte"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Écrire un fichier texte est une façon pour les programmeurs de stocker des informations sous forme de texte, ce qui peut être plus facile à lire et à manipuler pour les ordinateurs que d'autres formats de données. Les programmeurs utilisent souvent des fichiers texte pour stocker des configurations, des données de test ou de l'information de débogage.

## Comment:

```arduino
#include <SD.h>
File myFile;

void setup (){
  myFile = SD.open("texte.txt", FILE_WRITE);
  if (myFile) {
    myFile.println("Ce texte est écrit dans le fichier.");
    myFile.close();
  }
}
```

## Profondeur:

L'utilisation de fichiers texte remonte aux premières étapes de l'informatique et est toujours une option populaire en programmation. Les alternatives à l'écriture de fichiers texte incluent l'utilisation de bases de données ou de structures de données plus complexes, mais cela peut être plus compliqué et nécessite souvent des outils supplémentaires. Pour implémenter l'écriture de fichiers texte dans Arduino, vous aurez besoin d'une carte SD et de la bibliothèque SD.h.

## Voir aussi:

Pour plus d'informations sur l'utilisation de fichiers texte dans Arduino, consultez [la documentation officielle](https://www.arduino.cc/reference/en/libraries/sd/). Vous pouvez également trouver des tutoriels et des exemples pratiques sur [le site Web de Arduino](https://www.arduino.cc/en/Tutorial/Files).