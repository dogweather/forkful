---
date: 2024-01-26 04:36:37.615153-07:00
description: "Les nombres complexes poss\xE8dent une partie r\xE9elle et une partie\
  \ imaginaire, g\xE9n\xE9ralement \xE9crites comme `a + bi`. Ils sont essentiels\
  \ pour certains projets\u2026"
lastmod: '2024-02-25T18:49:54.777827-07:00'
model: gpt-4-0125-preview
summary: "Les nombres complexes poss\xE8dent une partie r\xE9elle et une partie imaginaire,\
  \ g\xE9n\xE9ralement \xE9crites comme `a + bi`. Ils sont essentiels pour certains\
  \ projets\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Les nombres complexes possèdent une partie réelle et une partie imaginaire, généralement écrites comme `a + bi`. Ils sont essentiels pour certains projets Arduino lourds en mathématiques impliquant le traitement des signaux, l'ingénierie électrique, ou tout autre domaine où les phénomènes sont mieux modélisés dans un plan.

## Comment faire :
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Démarrer la communication série
  
  Complex myComplex(2, 3); // Créer un nombre complexe 2 + 3i
  Complex anotherComplex(1, 1); // Créer un autre nombre complexe 1 + 1i
  
  // Addition
  Complex result = myComplex + anotherComplex; 
  Serial.print("Addition : "); 
  result.print(); // Affiche 3 + 4i
  
  // Multiplication
  result = myComplex * anotherComplex; 
  Serial.print("Multiplication : ");
  result.print(); // Affiche -1 + 5i
}

void loop() {
  // Non utilisé dans cet exemple
}
```
Sortie de l'exemple :
```
Addition : 3 + 4i
Multiplication : -1 + 5i
```

## Approfondissement
Initialement, les nombres complexes ont été accueillis avec scepticisme, mais ils sont devenus centraux dans divers domaines scientifiques. Historiquement, ils ont été reconnus pour fournir des solutions à des équations polynomiales qui manquent de solutions réelles.

Arduino n'inclut pas les nombres complexes dans sa bibliothèque standard, mais vous pouvez tirer parti de bibliothèques comme `Complex.h` pour les manipuler. À l'intérieur, ces bibliothèques définissent une classe Complex, utilisant typiquement deux doubles pour stocker les parties réelle et imaginaire, et surcharger les opérateurs pour supporter l'arithmétique.

Comme alternative, pour des applications qui n'ont pas intrinsèquement besoin de l'arithmétique des nombres complexes, envisagez d'utiliser d'autres stratégies mathématiques ou bibliothèques. Rappelez-vous, cependant, qu'utiliser des floats au lieu de nombres complexes pourrait oversimplifier certains problèmes.

## Voir aussi
- La bibliothèque [Complex.h](https://github.com/RobTillaart/Complex) de Rob Tillaart.
- Un approfondissement sur [la mathématique derrière les nombres complexes](https://mathworld.wolfram.com/ComplexNumber.html).
