---
title:                "Arduino: Suppression de caractères correspondant à un motif"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez avec Arduino, vous pourriez vous retrouver dans une situation où vous avez besoin de supprimer des caractères correspondant à un certain modèle. Cela peut sembler étrange, mais ça peut être utile dans des situations spécifiques, comme lorsqu'on traite des données reçues par un port série ou lorsqu'on effectue des opérations de nettoyage sur des chaînes de caractères.

## Comment faire

Pour supprimer des caractères correspondant à un modèle, nous allons utiliser la fonction `remove` de la bibliothèque `String`. Cette fonction prend deux paramètres : le premier est le caractère ou le motif à supprimer, et le deuxième est l'indice de départ à partir duquel la suppression doit commencer.

Voici un exemple de code qui utilise la fonction `remove` pour supprimer tous les caractères "a" d'une chaîne de caractères :

```Arduino
#include <String.h>

String exampleString = "Bonjour, Arduino !";
exampleString.remove('a', 0);
Serial.print(exampleString);
```

Ce code va imprimer la chaîne de caractères "Bonjour, rduino !" (notez que le "a" a été supprimé). Si nous voulons supprimer tous les caractères "o" à partir de l'indice 5, nous pouvons modifier notre code comme ceci :

```Arduino
exampleString.remove('o', 5);
```

Et nous obtiendrons "Bonjour, Ardinu !" comme résultat.

Il est également possible de supprimer plusieurs caractères à la fois en utilisant un modèle. Par exemple, si nous voulons supprimer tous les chiffres d'une chaîne de caractères, nous pouvons utiliser la fonction `remove` ainsi :

```Arduino
exampleString.remove("[0-9]");
```

Et cela supprimera tous les chiffres de la chaîne de caractères.

## Plongée en profondeur

La fonction `remove` utilise des expressions régulières pour supprimer des caractères correspondant à un motif spécifique. Les expressions régulières sont un moyen puissant de manipuler et de rechercher des chaînes de caractères dans un texte. Elles peuvent sembler intimidantes au début, mais une fois que vous aurez compris leur fonctionnement, elles peuvent être très utiles pour effectuer des opérations complexes de traitement de texte.

Par exemple, voici quelques expressions régulières couramment utilisées :

- `[0-9]` : correspond à n'importe quel chiffre
- `[a-z]` : correspond à n'importe quelle lettre minuscule
- `[A-Z]` : correspond à n'importe quelle lettre majuscule
- `.` : correspond à n'importe quel caractère
- `+` : correspond à une ou plusieurs occurrences du caractère précédent
- `*` : correspond à zéro ou plusieurs occurrences du caractère précédent

Il existe de nombreux autres caractères spéciaux et possibilités de combinaison, mais cela nécessiterait un article entier dédié aux expressions régulières.

## Voir aussi

- [La documentation officielle de la fonction `remove` de la bibliothèque `String`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/remove/)
- [Un tutoriel complet sur les expressions régulières en C++](https://www.cplusplus.com/reference/regex/)
- [Un outil en ligne pour tester vos expressions régulières](https://regex101.com/)