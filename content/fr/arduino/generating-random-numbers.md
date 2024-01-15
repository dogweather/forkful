---
title:                "Génération de nombres aléatoires."
html_title:           "Arduino: Génération de nombres aléatoires."
simple_title:         "Génération de nombres aléatoires."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un passionné d'électronique ou un bricoleur qui aime expérimenter avec l'Arduino, vous pourriez être intéressé par la génération de nombres aléatoires. Cela peut être utile pour créer des jeux, des concours ou même pour ajouter une touche de hasard dans vos projets.

## Comment faire

Générer des nombres aléatoires en utilisant l'Arduino est assez simple. Tout d'abord, vous devez inclure la bibliothèque "Random.h" dans votre code. Ensuite, vous pouvez utiliser la fonction random() pour générer des nombres aléatoires dans une plage spécifique.

```Arduino
#include <Random.h>

int nombreAleatoire = 0;

void setup() {
  Serial.begin(9600);
}

void loop() {
  nombreAleatoire = random(0, 10); // génère des nombres aléatoires entre 0 et 10
  Serial.println(nombreAleatoire); // affiche le nombre aléatoire dans le moniteur série
  delay(1000); // attend une seconde avant de générer un nouveau nombre
}
```

Dans cet exemple, nous avons utilisé la fonction random() pour générer un nombre aléatoire dans la plage de 0 à 10 et l'avons affiché sur le moniteur série toutes les secondes. Vous pouvez également utiliser la fonction randomSeed() pour initialiser la séquence de nombres aléatoires avec une valeur spécifique.

## Plongée en profondeur

L'Arduino utilise un générateur de nombres pseudo-aléatoires, ce qui signifie qu'il suit une séquence prévisible de nombres en fonction de la valeur utilisée pour initialiser la séquence. Cependant, pour la plupart des projets, cela suffit amplement.

Si vous voulez générer des nombres vraiment aléatoires, vous pouvez utiliser un circuit externe, tel qu'un générateur de bruit blanc, pour fournir une entrée aléatoire à l'Arduino. Cela peut être utile pour les projets de cybersécurité ou de cryptographie.

## Voir aussi

- [Documentation officielle sur la génération de nombres aléatoires avec Arduino](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Article sur la génération de nombres aléatoires réels avec l'Arduino](https://create.arduino.cc/projecthub/circuito/io-103-how-to-create-real-random-numbers-with-arduino-0795ef)
- [Vidéo sur la génération de nombres aléatoires avec l'Arduino](https://www.youtube.com/watch?v=8Xq9CAX0YkM)