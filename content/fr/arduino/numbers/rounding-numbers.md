---
title:                "Arrondir les nombres"
aliases:
- /fr/arduino/rounding-numbers/
date:                  2024-01-26T03:43:01.150933-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Arrondir des nombres consiste à réduire un décimal à sa valeur entière la plus proche ou à un nombre défini de décimales. Les programmeurs arrondissent les nombres pour les rendre plus faciles à lire et à manipuler, en particulier lorsque la précision au-delà d'un certain point n'est pas nécessaire ou pourrait entraîner des erreurs.

## Comment faire :
Avec Arduino, vous pouvez arrondir les nombres en utilisant des fonctions intégrées. Les principaux acteurs sont `round`, `ceil` et `floor`. Voici une démo rapide :

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // Arrondir au nombre entier le plus proche
  Serial.println(round(myNumber)); // Résultats : 123

  // Arrondit toujours vers le haut
  Serial.println(ceil(myNumber));  // Résultats : 124

  // Arrondit toujours vers le bas
  Serial.println(floor(myNumber)); // Résultats : 123
}

void loop() {
  // Rien à parcourir en boucle.
}
```

## Plongée en profondeur :
Les algorithmes d'arrondissement ont une longue histoire ; ils existent bien avant les ordinateurs numériques. Dans le calcul analogique, l'arrondissement était un processus physique. Dans le calcul numérique, c'est un processus mathématique.

L'arrondissement est nécessaire lorsque nous convertissons d'un type avec plus de précision (comme `float` ou `double`) à un type avec moins de précision (comme `int`). Mais la manière d'arrondir peut varier :

1. `round()`: Arrondissement standard. Si la fraction est de 0,5 ou plus, cela arrondit à l'entier supérieur ; sinon, cela arrondit à l'entier inférieur.
2. `ceil()`: Abréviation pour "plafond", arrondit toujours vers le haut au nombre entier le plus proche, même s'il est plus proche du nombre inférieur.
3. `floor()`: Le contraire de plafond ; arrondit toujours vers le bas.

Le choix entre ces fonctions dépend de l'objectif de la valeur arrondie. Les mesures peuvent nécessiter un arrondissement standard, l'argent utilise souvent `floor`, tandis que les systèmes d'inventaire pourraient utiliser `ceil` pour s'assurer que tout est pris en compte.

L'implémentation de ces fonctions par Arduino est simple ; elles ne gèrent pas de cas supplémentaires comme l'arrondissement à des décimales spécifiques. Pour cela, une fonction personnalisée ou des mathématiques plus approfondies entrent en jeu - pensez à multiplier pour décaler la décimale, arrondir, puis diviser à nouveau.

Les erreurs d'arrondi peuvent s'accumuler, ayant un impact significatif sur les calculs longs ou les processus itératifs. Les programmeurs doivent être prudents lorsqu'ils effectuent de nombreuses opérations sur des valeurs arrondies.

## Voir aussi :
2. Un regard approfondi sur les pièges et les stratégies d'arrondissement : [Guide des points flottants](https://floating-point-gui.de/)
3. Pour des techniques avancées, y compris des fonctions d'arrondissement personnalisées et la gestion des erreurs d'arrondi, vous pourriez consulter des ressources académiques ou des guides de programmation détaillés.
