---
title:                "Utilisation des tableaux associatifs"
aliases:
- /fr/bash/using-associative-arrays.md
date:                  2024-01-30T19:10:07.783175-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les tableaux associatifs sont comme des tableaux surpuissants qui vous permettent d'utiliser des chaînes de caractères comme indices, au lieu de se limiter aux entiers. Les programmeurs les utilisent pour des structures de données plus complexes, facilitant la gestion de données qui ne s'insèrent pas facilement dans une liste séquentielle.

## Comment faire :

D'abord, déclarez un tableau associatif en Bash :

```Bash
declare -A my_array
```

Ensuite, vous pouvez commencer à le remplir avec des valeurs, en utilisant des chaînes de caractères comme clés :

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programming"
```

Pour accéder à un élément, utilisez sa clé :

```Bash
echo ${my_array["name"]}  # Affiche : Linux Journal
```

Il est également simple d'itérer sur les clés et les valeurs :

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

Un exemple de sortie pourrait ressembler à ceci :

```
name: Linux Journal
topic: Programming
```

Pour ajouter ou modifier des éléments, assignez simplement une valeur à une clé, de manière similaire au remplissage initial :

```Bash
my_array["readers"]="Vous"
```

Et pour supprimer un élément, utilisez `unset` :

```Bash
unset my_array["topic"]
```

## Plongée en profondeur

Les tableaux associatifs ont été introduits dans la version 4.0 de Bash, ce qui en fait une addition relativement récente au langage. Avant leur introduction, la gestion des tableaux à index non entier était laborieuse, nécessitant souvent des solutions de contournement ou des outils externes comme `awk` ou `sed`.

Sous le capot, Bash implémente les tableaux associatifs en utilisant des tables de hachage. Cette implémentation permet une recherche de clé efficace, qui reste relativement constante quelle que soit la taille du tableau, une caractéristique cruciale pour la performance lors de l'exécution de scripts.

Alors que les tableaux associatifs en Bash apportent beaucoup de puissance et de flexibilité aux scripts shell, ils viennent avec leur propre ensemble de limitations, telles qu'une manipulation quelque peu plus maladroite par rapport aux tableaux dans des langues de plus haut niveau comme Python ou JavaScript. Pour des tâches de manipulation de données complexes, il pourrait encore être judicieux d'envisager des outils ou des langues externes mieux adaptés à la tâche.

Cependant, pour de nombreuses tâches de script typiques, les tableaux associatifs fournissent un outil précieux dans la boîte à outils du programmeur Bash, permettant des scripts plus lisibles et maintenables en autorisant l'utilisation de clés de chaîne de caractères significatives au lieu d'indices numériques.
