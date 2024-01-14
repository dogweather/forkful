---
title:    "Gleam: Extraction de sous-chaînes"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Pourquoi

L'extraction de sous-chaînes est une technique utile pour manipuler les chaînes de caractères dans vos programmes Gleam. Cela peut vous aider à extraire des informations spécifiques d'une chaîne plus grande, ou à créer des sous-chaînes à partir d'une seule source.

## Comment faire

Pour extraire une sous-chaîne, utilisez la fonction `substring(start, end, string)` en remplaçant `start` et `end` par les index de début et de fin de la sous-chaîne que vous souhaitez extraire, et `string` par la chaîne complète.

Exemple :

```Gleam
let ma_chaine = "Bonjour tout le monde!"

let sous_chaine = substring(8, 12, ma_chaine)

lancer(sous_chaine)
```

Sortie :
```
tout
```

Pour extraire une sous-chaîne à partir de la fin de la chaîne, vous pouvez utiliser des valeurs négatives pour les index de début et de fin. Par exemple, `substring(-5, -1, ma_chaine)` extraira la sous-chaîne des 5 derniers caractères.

## Plongée en profondeur

L'extraction de sous-chaînes utilise les index pour sélectionner une partie spécifique d'une chaîne. Les index commencent toujours à 0, ce qui signifie que le premier caractère d'une chaîne a un index de 0, le deuxième a un index de 1 et ainsi de suite.

De plus, l'index de fin est exclusif, ce qui signifie que le caractère à l'index de fin ne sera pas inclus dans la sous-chaîne résultante. Par exemple, `substring(0, 3, ma_chaine)` retournera les caractères de 0 à 2 (donc les 3 premiers caractères).

Il est également important de prendre en compte les caractères spéciaux et les accents lors de l'extraction de sous-chaînes, car ils peuvent avoir un impact sur les index des caractères.

# À voir aussi

- [La documentation officielle sur l'extraction de sous-chaînes en Gleam](https://gleam.run/documentation/stdlib/string#substring)
- [Un tutoriel vidéo sur l'utilisation de la fonction substring](https://www.youtube.com/watch?v=H6ZJ0D5wOrg)
- [Exemples d'utilisation de l'extraction de sous-chaînes en Gleam](https://github.com/username/repo)