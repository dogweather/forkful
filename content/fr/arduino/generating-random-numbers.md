---
title:                "Arduino: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une tâche courante dans la programmation Arduino. Cela peut être utile pour des jeux, des simulations ou simplement pour ajouter un élément de hasard à vos projets.

## Comment faire

Pour générer des nombres aléatoires dans votre code Arduino, utilisez la fonction `random(min, max)`. Elle génèrera un nombre compris entre `min` et `max`-1. Si vous avez besoin d'un nombre décimal, vous pouvez utiliser la fonction `random(min, max, precision)` où `precision` est le nombre de chiffres après la virgule.

Voici un exemple de code qui génère un nombre aléatoire entre 0 et 10 et l'affiche sur le moniteur série :

```Arduino
void setup(){
    Serial.begin(9600);
}

void loop(){
    int nombre = random(0, 10);
    Serial.println(nombre);
    delay(1000);
}
```

La sortie sera quelque chose comme ceci :

```
3
8
2
5
...
```

## Plongée en profondeur 

Pour générer des nombres aléatoires, l'Arduino utilise un algorithme appelé congruential generator, qui utilise une formule mathématique complexe pour générer une série de nombres pseudo-aléatoires. Son efficacité dépend des valeurs choisies pour les paramètres `a`, `c` et `m` dans la formule. Ces valeurs ont été soigneusement sélectionnées pour assurer un équilibre entre l'imprévisibilité et l'efficacité de l'algorithme.

Il est important de noter que ces nombres sont en fait pseudo-aléatoires, ce qui signifie qu'il y a en fait un modèle prévisible dans la série de nombres générée. Cependant, pour la plupart des applications, ces nombres seront suffisamment aléatoires pour être utilisés.

## Voir aussi

Pour en savoir plus sur la génération de nombres aléatoires sur Arduino, vous pouvez consulter les liens suivants :

- [Documentation officielle sur la fonction `random()`](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Article sur la génération de nombres aléatoires sur Arduino](https://www.makerguides.com/random-numbers-arduino/)
- [Vidéo sur les nombres aléatoires en informatique](https://www.youtube.com/watch?v=SxP30euw3-0)