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

# Pourquoi

Les extractions de sous-chaînes sont utiles lorsqu'on souhaite récupérer une partie d'une chaîne de caractères, comme un numéro de téléphone ou une adresse email, sans avoir à parcourir toute la chaîne de caractères.

# Comment faire

Pour extraire une sous-chaîne en TypeScript, on utilise la méthode `substring()` sur la chaîne de caractères, en lui passant en paramètres l'index de début et de fin de la sous-chaîne souhaitée (en comptant à partir de zéro).

```TypeScript
let str: string = "Bonjour tout le monde";
let subStr: string = str.substring(8, 12);

console.log(subStr); // affiche "tout"
```

# Plongée en profondeur

Il est important de comprendre que la méthode `substring()` ne modifie pas la chaîne originale, elle renvoie simplement la sous-chaîne demandée. De plus, si on ne spécifie pas l'index de fin, la sous-chaîne renvoyée va jusqu'à la fin de la chaîne originale.

On peut également utiliser des index négatifs pour spécifier un comptage à partir de la fin de la chaîne. Par exemple, `substring(-6)` renverra les 6 derniers caractères de la chaîne.

# Voir aussi

- Documentation officielle de `substring()` en TypeScript: https://www.typescriptlang.org/docs/handbook/strings.html#substring
- Référence complète des méthodes sur les chaînes de caractères en TypeScript: https://www.w3schools.com/jsref/jsref_obj_string.asp