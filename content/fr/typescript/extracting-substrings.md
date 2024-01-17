---
title:                "Extraction de sous-chaînes"
html_title:           "TypeScript: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi on le fait ?

Extraire des sous-chaînes est une méthode couramment utilisée en programmation qui consiste à récupérer une partie d'une chaîne de caractères. Les programmeurs utilisent cette technique pour traiter des données textuelles et effectuer des opérations de manipulation de texte plus précises et ciblées.

# Comment faire :

Voici quelques exemples de code en TypeScript pour extraire des sous-chaînes :

```TypeScript
// Exemple 1 : Extraire une sous-chaîne à partir d'une position de départ donnée 
let string = "Bonjour tout le monde !";
let subString = string.substring(8); // Résultat : "tout le monde !"

// Exemple 2 : Extraire une sous-chaîne en spécifiant une position de départ et une position d'arrivée
let string = "Bonjour tout le monde !";
let subString = string.substring(8, 12); // Résultat : "tout"

// Exemple 3 : Extraire une sous-chaîne en utilisant un motif de caractères spécifique
let string = "Bonjour les programmeurs !";
let subString = string.match(/programmeurs/); // Résultat : "programmeurs"
```

# Zoom sur :

## Contexte historique :
L'extraction de sous-chaînes a été rendue populaire par le langage de programmation Perl dans les années 1980. Depuis, cette technique a été implémentée dans de nombreux autres langages, dont TypeScript.

## Alternatives :
En plus de la méthode `substring`, TypeScript offre également les méthodes `slice` et `substr` pour extraire des sous-chaînes. Ces trois méthodes ont des fonctionnalités similaires mais présentent quelques différences dans leurs arguments et leurs comportements.

## Détails d'implémentation :
La méthode `substring` prend deux arguments optionnels : le premier représente la position de départ de la sous-chaîne et le second représente la position d'arrivée de la sous-chaîne. Si le second argument est omis, la sous-chaîne sera extraite à partir de la position de départ jusqu'à la fin de la chaîne. Si les deux arguments sont spécifiés, la sous-chaîne sera extraite entre ces deux positions incluses.

# Voir aussi :

Pour en savoir plus sur l'extraction de sous-chaînes en TypeScript, vous pouvez consulter la documentation officielle de TypeScript à ce sujet : [Documentation de TypeScript sur les sous-chaînes](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-subtraction).