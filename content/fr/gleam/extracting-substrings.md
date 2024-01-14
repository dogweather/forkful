---
title:                "Gleam: Extraction de sous-chaînes"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une pratique courante en programmation pour obtenir une partie spécifique d'une chaîne de caractères. Cela peut être utile lorsque vous avez besoin d'analyser ou de manipuler une chaîne de données plus grande. Dans cet article, nous allons voir comment utiliser la bibliothèque Gleam pour extraire des sous-chaînes en toute simplicité.

## Comment Faire

Pour extraire une sous-chaîne, vous devez utiliser la fonction `substring` de la bibliothèque Gleam. Cette fonction prend trois arguments : la chaîne de départ, l'index de début et l'index de fin. Voici un exemple de code qui montre comment extraire une sous-chaîne à partir d'une chaîne donnée :

```Gleam
import gleam/strings

// Définir la chaîne d'origine
let chaine = "Bonjour le monde"

// Extraire de l'index 8 à la fin de la chaîne
let sous_chaine = strings.substring(chaine, 8, strings.length(chaine))

// Afficher la sous-chaîne
io.println(sous_chaine)
```

Dans cet exemple, la sortie sera `le monde`, car nous avons extrait la portion de la chaîne à partir de l'index 8 jusqu'à la fin.

Vous pouvez également donner un index de début négatif pour extraire une sous-chaîne à partir de la fin de la chaîne. Par exemple, si vous voulez extraire les 5 derniers caractères d'une chaîne, vous pouvez utiliser `-5` comme index de début.

```Gleam
let chaine = "Bienvenue sur Gleam"

// Extraire les 5 derniers caractères
let sous_chaine = strings.substring(chaine, -5, strings.length(chaine))

// Afficher la sous-chaîne
io.println(sous_chaine)
```

La sortie sera `Gleam`, car nous avons extrait les 5 derniers caractères de la chaîne.

## Plongée Profonde

La fonction `substring` fonctionne en créant un nouveau tableau de caractères à partir de la chaîne donnée. Cette méthode peut sembler inefficace pour les grandes chaînes, car elle nécessite de copier les caractères dans un nouveau tableau. Cependant, grâce à la gestion automatique de la mémoire par Gleam, cette opération n'a pas un impact significatif sur les performances.

Il est également important de noter que la fonction `substring` utilise les indices UTF-8 pour extraire les sous-chaînes. Cela signifie que les caractères multibits sont correctement gérés et que la fonction ne coupera pas un caractère en deux lors de l'extraction d'une sous-chaîne.

## Voir Aussi

- Documentation officielle de la bibliothèque Gleam Strings : https://gleam.run/modules/strings.html
- Tutoriel sur les chaînes de caractères en Gleam : https://medium.com/analytics-vidhya/gleaming-with-gleam-strings-1533e2507dd1
- Exemples de code pour les fonctions de manipulation de chaînes : https://github.com/gleam-lang/gleam/blob/master/lib/strings/src/strings.gleam