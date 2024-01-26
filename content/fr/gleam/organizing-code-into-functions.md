---
title:                "Organisation du code en fonctions"
date:                  2024-01-26T01:10:02.886651-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Organiser le code en fonctions signifie décomposer le comportement d'un programme en morceaux plus petits et réutilisables. Les programmeurs font cela pour rendre le code plus clair, plus maintenable et pour éviter la répétition.

## Comment faire :
Voici un exemple simple d'organisation du code en fonctions en Gleam :

```gleam
fn add(x, y) {
  x + y
}

fn main() {
  let sum = add(3, 4)
  sum
}

// Sortie d'exemple
// 7
```

Dans cet extrait, `add` est une fonction qui prend deux valeurs et les additionne. `main` est l'endroit où nous appelons `add` et gérons le résultat.

## Plongée en profondeur
Historiquement, le concept de fonctions (ou « sous-routines ») a révolutionné la programmation, ouvrant la voie à la programmation structurée dans les années 1960 et au-delà. Les fonctions encouragent une approche modulaire, où les problèmes sont divisés en sous-problèmes, résolus indépendamment, et composés pour résoudre le problème plus large.

En Gleam, qui est fortement typé, les fonctions portent également des informations de type, assurant que leur utilisation est cohérente avec leur définition. Cela réduit les erreurs et clarifie les intentions.

Les alternatives aux fonctions incluent le codage en ligne, où la logique est écrite de manière répétée. Bien que parfois plus rapide pour de petites tâches ponctuelles, le codage en ligne ne passe pas bien à l'échelle pour des applications plus importantes.

Les détails d'implémentation à considérer lors de l'organisation en fonctions peuvent inclure la composition de fonctions, où les fonctions sont utilisées comme blocs de construction, et les fonctions d'ordre supérieur, qui prennent d'autres fonctions comme arguments ou les retournent, ajoutant de la flexibilité à la manière dont le code est organisé et exécuté.

## Voir Aussi
Pour en savoir plus sur les fonctions en Gleam, vous pouvez plonger dans la documentation officielle à :
- [Fonctions dans le langage Gleam](https://gleam.run/book/tour/functions.html)

Ou explorez des concepts de programmation plus larges :
- [Réseau des développeurs Mozilla sur les fonctions JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Fonctions)
- [Apprenez-vous un peu d'Erlang pour un grand bien ! - Sur les modules et les fonctions](https://learnyousomeerlang.com/modules)