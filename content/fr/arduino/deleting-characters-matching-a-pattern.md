---
title:                "Supprimer les caractères correspondant à un modèle"
html_title:           "Arduino: Supprimer les caractères correspondant à un modèle"
simple_title:         "Supprimer les caractères correspondant à un modèle"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que la suppression de caractères correspondant à un modèle et pourquoi les programmeurs le font-ils?

La suppression de caractères correspondant à un modèle est une technique de traitement de texte qui consiste à rechercher dans une chaîne de caractères des caractères qui correspondent à un motif donné, puis à les supprimer. Les programmeurs utilisent cette technique pour nettoyer et formater des chaînes de caractères, en éliminant des éléments indésirables tels que les espaces, les caractères spéciaux, ou en conservant uniquement certaines parties d'une chaîne.

## Comment faire:

Voici deux exemples de code pour supprimer des caractères correspondant à un motif dans une chaîne de caractères en utilisant Arduino:

```
// Supprimer les espaces dans une chaîne de caractères
String chaine = "Ceci est une chaîne de caractères avec des espaces";
chaine.replace(" ", "");

// Supprimer les caractères spéciaux dans une chaîne de caractères
String chaine = "Ceci#est%une$chain&de@caractères";
chaine.replace("#", "");
chaine.replace("%", "");
chaine.replace("$", "");
chaine.replace("&", "");
chaine.replace("@", "");
```

Résultats:

```
CeciEstUneChaîneDeCaractèresAvecDesEspaces
CeciEstUneChaîneDeCaractères
```

## Plongée en profondeur:

La suppression de caractères correspondant à un motif est une technique couramment utilisée dans les langages de programmation, tels que Python et C++, pour manipuler des données textuelles. Avant l'introduction de cette technique, les programmeurs devaient utiliser des boucles pour parcourir une chaîne de caractères et supprimer manuellement les caractères correspondant à un motif. Avec la suppression de caractères correspondant à un motif, cette tâche peut être réalisée en une seule ligne de code, ce qui rend le processus plus efficace et plus rapide.

Il existe également d'autres techniques pour supprimer des caractères correspondant à un motif, telles que l'utilisation d'expressions régulières. Cependant, la suppression de caractères correspondant à un motif est généralement préférée pour sa simplicité et sa facilité d'utilisation.

En termes d'implémentation dans Arduino, la fonction ```replace()``` est disponible pour les objets de type ```String``` et prend en paramètre le motif à supprimer ainsi que la chaîne de remplacement. Elle peut également être appliquée plusieurs fois sur une même chaîne pour supprimer différents motifs.

## Voir aussi:

- [La documentation Arduino pour la fonction replace()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Un guide sur l'utilisation des expressions régulières en C++](https://www.cprogramming.com/tutorial/regular-expressions.html)