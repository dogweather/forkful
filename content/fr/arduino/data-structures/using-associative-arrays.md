---
title:                "Utilisation des tableaux associatifs"
aliases:
- /fr/arduino/using-associative-arrays.md
date:                  2024-01-30T19:10:22.726335-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Dans le domaine de l'Arduino, les tableaux associatifs vous permettent d'associer des clefs à des valeurs, un peu comme si vous associez des chaussettes avec leurs paires. Ils sont privilégiés lorsque vous devez stocker et récupérer des données en utilisant des noms descriptifs, rendant votre code plus propre et bien plus compréhensible.

## Comment faire :
Strictement parlant, l'Arduino ne possède pas de support intégré pour les tableaux associatifs comme vous pourriez en trouver dans des langages de plus haut niveau. Mais, n'ayez crainte. Nous pouvons être ingénieux en utilisant des structures et des tableaux pour imiter cette fonctionnalité. Voici un exemple simple pour créer un "tableau associatif" basique pour stocker et accéder aux températures de différentes villes.

Premièrement, définissez une structure pour contenir la ville (clef) et sa température (valeur) :

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Ensuite, initialisez un tableau d’objets `CityTemperature` :

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Voici comment vous pouvez accéder à la température d'une ville spécifique et l'afficher :

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("La température à Los Angeles est : ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Rien ici pour le moment.
}
```

Exécuter ce code vous donnera la sortie :

```
La température à Los Angeles est : 22.0
```

## Plongée en profondeur
Historiquement, les langages de programmation comme C et C++ (d'où est dérivée la syntaxe de l'Arduino) ne venaient pas avec des tableaux associatifs intégrés, menant à des solutions de contournement comme celle montrée ci-dessus. Cette approche est relativement simple mais s'échelonne mal à mesure que la taille des données augmente en raison de son temps de recherche en O(n).

Des langages tels que Python proposent des dictionnaires, et JavaScript dispose d'objets à cet effet, tous deux étant bien plus efficaces pour gérer les paires clé-valeur. Sur Arduino, lorsque la performance et l'efficacité deviennent critiques, les développeurs pourraient opter pour des structures de données plus spécialisées, comme les tables de hachage, implémentées via des bibliothèques.

Bien qu'Arduino ne prenne pas en charge nativement les tableaux associatifs, la communauté a développé des bibliothèques comme `HashMap` qui peuvent être ajoutées à votre projet pour fournir une fonctionnalité similaire avec une meilleure performance qu'une approche DIY. Ces bibliothèques offrent généralement un moyen plus élégant et efficace de gérer les tableaux associatifs, en particulier pour des projets plus complexes.
