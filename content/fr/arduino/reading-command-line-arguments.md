---
title:                "Lire les arguments de ligne de commande"
html_title:           "Arduino: Lire les arguments de ligne de commande"
simple_title:         "Lire les arguments de ligne de commande"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?

Lire des arguments de ligne de commande est un moyen pour les programmeurs de récupérer des informations saisies par l'utilisateur lors de l'exécution de leur programme. Cela peut être utile pour personnaliser l'exécution du programme en fonction des préférences de l'utilisateur.

## Comment faire :

Voici un exemple de code pour lire un argument de ligne de commande dans Arduino :

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available()) {
    String arg = Serial.readString(); //lit l'argument
    Serial.println("Argument saisi : " + arg); //affiche l'argument
  }
}
```

Lors de l'exécution de ce code, l'utilisateur peut saisir un argument dans le moniteur série et celui-ci sera affiché à l'écran.

Exemple d'entrée utilisateur : "Hello World!"

Sortie attendue : "Argument saisi : Hello World!"

## Plongée en profondeur :

Les arguments de ligne de commande sont basés sur le concept plus général des paramètres de la ligne de commande, qui a été introduit dans les systèmes d'exploitation Unix dans les années 1960. Les programmeurs peuvent également utiliser des bibliothèques ou des fonctions spécifiques pour lire des arguments dans Arduino, comme la bibliothèque "CmdParser" ou la fonction "getopt()".

## À voir aussi :

Pour en savoir plus sur l'utilisation des arguments de ligne de commande dans Arduino, vous pouvez consulter ces ressources :

- [Documentation officielle Arduino sur la lecture des arguments de ligne de commande](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/)
- [Exemple de projet utilisant des arguments de ligne de commande dans Arduino](https://github.com/taholmes/arduino-gpio/tree/master/examples/03-commandlineargs)
- [Vidéo explicative sur les arguments de ligne de commande dans Arduino](https://www.youtube.com/watch?v=0Y9kyChItGU)