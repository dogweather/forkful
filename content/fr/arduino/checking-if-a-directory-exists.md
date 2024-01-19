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

## Quoi et Pourquoi ?

La vérification de l'existence d'un répertoire est une action qui permet de s'assurer qu'un certain dossier ou répertoire existe bien dans votre système de fichiers. Les programmeurs font cela pour empêcher des erreurs lors de la tentative d'accéder à des répertoires qui n'existent pas.

## Comment Faire :

Malheureusement, Arduino ne prend pas en charge directement la vérification de l'existence d'un répertoire comme certains autres langages de programmation. Cependant, on pourrait le faire après avoir configuré un module de carte SD. Voici un exemple de comment cela pourrait être réalisé:

```Arduino
#include <SPI.h>
#include <SD.h>

File racine;

void setup ()
{
    Serial.begin (9600);
    if (!SD.begin (4)) {
        Serial.println ("Erreur d'initialisation de la carte SD");
        return;
    }
    racine = SD.open ("/");

    if (!racine) {
      Serial.println("Le répertoire n'existe pas.");
    } else {
      Serial.println("Le répertoire existe."); 
    }
}
```

## Plongée en Profondeur 

Les langages de programmation modernes, y compris Arduino, ne prennent généralement pas en charge la vérification de l'existence d'un répertoire de manière native. Cependant, en utilisant des bibliothèques supplémentaires comme le module SD dans l'exemple ci-dessus, les développeurs Arduino peuvent introduire cette fonctionnalité.

Il existe des alternatives à l'utilisation du module SD, notamment en utilisant le module ESP8266FS, qui contribue également à la gestion de fichiers sur Arduino.

En ce qui concerne les détails de l'implémentation, Arduino ouvre un fichier via la méthode `SD.open` et renvoie un objet `File`. Si la tentative d'ouvrir le fichier échoue (ce qui peut être dû au fait que le fichier n'existe pas), l'objet `File` retourné ne sera pas valide. Vous pouvez alors vérifier si le fichier est valide en utilisant `if (! maFichier)`.

## À Voir Aussi 

Vous pouvez trouver plus d'informations sur la gestion des fichiers sur Arduino en visitant ces liens:

1. Documentation Arduino sur la gestion de fichiers avec une carte SD : https://www.arduino.cc/en/Reference/SD
2. Un guide sur la gestion des fichiers sur Arduino utilisant ESP8266FS : https://randomnerdtutorials.com/install-esp8266-filesystem-uploader-arduino-ide/
3. Explication détaillée des objets `File`: https://www.arduino.cc/en/Reference/File