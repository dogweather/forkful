---
title:                "Générer des nombres aléatoires"
html_title:           "Arduino: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Génération de nombres aléatoires sur Arduino

## Quoi & pourquoi?

L'utilisation de nombres aléatoires est une méthode courante en programmation pour générer des valeurs aléatoires pour diverses applications. Cela peut être utile pour simuler des situations aléatoires telles que des jeux de hasard, pour générer des données pour des tests ou pour apporter une touche de variété dans un programme. Les programmeurs utilisent la génération de nombres aléatoires pour rendre leurs programmes plus dynamiques et plus intéressants.

## Comment faire:

Générer des nombres aléatoires sur Arduino est très simple. Tout d'abord, nous devons déclarer une variable pour stocker le nombre aléatoire, puis nous utilisons la fonction `random(min, max)` pour générer un nombre aléatoire compris entre la valeur minimale et la valeur maximale spécifiées. Par exemple, si nous voulons générer un nombre aléatoire entre 0 et 9, nous pouvons utiliser la fonction suivante:

```Arduino
int randomNum = random(0, 10);
```

Vous pouvez également utiliser la fonction `random(max)` si vous souhaitez simplement générer un nombre aléatoire compris entre 0 et un nombre spécifié. Par exemple, si vous voulez un nombre aléatoire entre 0 et 100, vous pouvez utiliser la fonction suivante:

```Arduino
int randomNum = random(101);
```

Ensuite, vous pouvez utiliser cette variable dans votre programme comme bon vous semble pour ajouter une touche de hasard.

## Plongée en profondeur:

La génération de nombres aléatoires sur Arduino est rendue possible grâce à l'utilisation d'un générateur de nombres pseudo-aléatoires. Cela signifie que les nombres ne sont pas vraiment aléatoires, mais plutôt générés à partir d'un algorithme qui produit des séquences de nombres qui semblent aléatoires. Ce processus est appelé "pseudo-aléatoire" car le résultat final peut être reproduit si les mêmes paramètres sont fournis.

Il existe différentes manières de générer des nombres aléatoires sur Arduino, en utilisant différentes bibliothèques telles que la bibliothèque `stdlib.h` ou la bibliothèque `random.h`. Chacune de ces bibliothèques utilise des algorithmes différents pour générer des nombres aléatoires, il est donc important de comprendre comment elles fonctionnent afin de choisir celle qui convient le mieux à votre programme.

## Voir aussi:

Vous pouvez en apprendre plus sur la génération de nombres aléatoires sur Arduino en consultant les liens suivants:

- [Documentation officielle pour la fonction random() sur Arduino](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Un tutoriel sur la génération de nombres aléatoires sur Arduino](https://learn.adafruit.com/random-numbers-with-arduino/arduino-random)
- [Un article expliquant le fonctionnement des générateurs de nombres pseudo-aléatoires](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)