---
title:    "Arduino: Lecture des arguments de ligne de commande"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Lors de la programmation d'Arduino, il est souvent nécessaire de recevoir des données d'entrée provenant de l'utilisateur ou d'un autre appareil. Les arguments de ligne de commande permettent aux utilisateurs de spécifier ces données au moment de l'exécution du programme, ce qui peut être utile dans de nombreuses situations.

## Comment faire

Pour lire les arguments de ligne de commande dans un programme Arduino, il faut utiliser la fonction `processCommand()` qui analyse et extrait les arguments. Elle retourne le nombre d'arguments présents ainsi que les valeurs sous forme de chaînes de caractères.

Voici un exemple de code montrant comment utiliser la fonction `processCommand()` et afficher les valeurs des arguments :

```Arduino
#include <stdio.h>

void setup() {
  Serial.begin(9600); // initialise la communication série
}

void loop() {
  int numberOfArguments; // variable pour stocker le nombre d'arguments
  char* argumentValues[10]; // tableau pour stocker les valeurs des arguments

  // Définit les valeurs des arguments
  char input[] = "1 2 3 4 5";
  
  // Appelle la fonction processCommand() en lui passant l'entrée en tant que paramètre
  numberOfArguments = processCommand(input, argumentValues);

  // Affiche le nombre d'arguments présents
  Serial.println("Nombre d'arguments : " + String(numberOfArguments));

  // Affiche les valeurs des arguments
  for (int i = 0; i < numberOfArguments; i++) {
    Serial.println("Argument " + String(i + 1) + " : " + argumentValues[i]);
  }

  delay(1000);
}

// Fonction processCommand() qui analyse les arguments de ligne de commande
int processCommand(char* input, char* values[]) {
  int argumentCount = 0; // variable pour stocker le nombre d'arguments actuels
  char* currentArgument = strtok(input, " "); // extrait le premier argument (jusqu'à l'espace)

  // tant qu'il y a un argument
  while (currentArgument != NULL) {
    values[argumentCount] = currentArgument; // ajoute l'argument actuel au tableau des valeurs
    argumentCount++; // incrémente le nombre d'arguments actuels
    currentArgument = strtok(NULL, " "); // extrait le prochain argument
  }

  return argumentCount; // retourne le nombre d'arguments présents
}
```

### Exemple de sortie :

```
Nombre d'arguments : 5
Argument 1 : 1
Argument 2 : 2
Argument 3 : 3
Argument 4 : 4
Argument 5 : 5
```

## Plongée en profondeur

Il est important de noter que la fonction `processCommand()` peut prendre en charge un nombre limité d'arguments (dans notre exemple, 10). Si plus d'arguments sont présents, ils ne seront pas pris en compte.

De plus, cette fonction ne peut lire que des arguments de type chaîne de caractères. Si vous avez besoin de lire des entiers, floats ou d'autres types de données, vous devrez utiliser des fonctions de conversion appropriées telles que `atoi()` ou `atof()`.

Il est également possible de spécifier des options ou des flags dans les arguments de ligne de commande, qui peuvent être utiles pour modifier le comportement du programme. Cependant, leur gestion peut être complexe et peut nécessiter l'utilisation d'une librairie dédiée.

## Voir aussi

- [Documentation officielle Arduino sur les arguments de ligne de commande](https://www.arduino.cc/en/Main/CommandLineArguments)
- [Tutoriel sur les arguments de ligne de commande pour Arduino](https://create.arduino.cc/projecthub/Arduino_Genuino/arduino-cli-process-command-line-arguments-5aadd1)
- [Article sur l'utilisation de librairies pour gérer les options et flags dans les arguments de ligne de commande Arduino](https://www.mathertel.de/Arduino/argscan/argscan.ino)