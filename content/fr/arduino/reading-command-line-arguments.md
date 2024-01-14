---
title:    "Arduino: La lecture d'arguments en ligne de commande."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Pour ceux qui cherchent à étendre leurs connaissances sur la programmation Arduino, la lecture des arguments en ligne de commande peut être une compétence utile à apprendre. Cela vous permet de contrôler votre programme via la ligne de commande, en donnant des arguments spécifiques pour personnaliser son comportement.

## Comment faire

La lecture des arguments en ligne de commande est assez simple à mettre en œuvre dans un programme Arduino. Tout d'abord, vous devez inclure la bibliothèque `Arduino.h` dans votre code, qui contient les fonctions nécessaires pour lire les arguments. Ensuite, vous pouvez utiliser la fonction `argsRead()` pour lire les arguments lors de l'exécution du programme. Voici un exemple de code avec un argument nommé "led" qui prend une valeur numérique :

```Arduino
#include <Arduino.h>

void setup(){
  Serial.begin(9600);
}

void loop(){
  int led = argsRead("led").toInt();
  Serial.println("Valeur de l'argument 'led' : ");
  Serial.println(led);
}
```

Si vous entrez "arduino-cli -p COM3 -- led=5" dans la ligne de commande, vous verrez l'impression suivante dans le moniteur série :

```
Valeur de l'argument 'led':
5
```

## Approfondir

Il y a plusieurs choses à prendre en compte lors de la lecture des arguments en ligne de commande pour les programmes Arduino. Tout d'abord, assurez-vous de prendre en compte les différentes valeurs possibles pour chaque argument, en utilisant des conditions ou des boucles pour gérer les cas spécifiques. De plus, vous pouvez utiliser des boucles pour traiter plusieurs arguments en même temps, en utilisant des noms d'arguments différents. Enfin, vous pouvez également utiliser des commandes conditionnelles pour définir des valeurs par défaut pour les arguments, au cas où ils ne seraient pas inclus dans la ligne de commande.

## Voir aussi

Pour en savoir plus sur la lecture des arguments en ligne de commande dans Arduino, voici quelques liens utiles :

- [Documentation officielle d'Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/argsread/)
- [Guide pour les débutants sur la lecture des arguments en ligne de commande](https://maker.pro/arduino/tutorial/how-to-read-command-line-arguments-arduino)

J'espère que cet article vous a été utile pour comprendre comment lire les arguments en ligne de commande dans vos programmes Arduino. Bonne programmation !