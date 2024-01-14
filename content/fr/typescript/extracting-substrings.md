---
title:                "TypeScript: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi extraire des sous-chaînes en TypeScript ?

Dans de nombreux projets de programmation, vous pouvez être confronté à la nécessité de manipuler des chaînes de caractères. Mais parfois, vous pouvez avoir besoin d'extraire une partie spécifique de cette chaîne, appelée sous-chaîne. Cela peut être utile lors de la manipulation de données ou de la validation des saisies utilisateur. En utilisant TypeScript, vous pouvez facilement extraire ces sous-chaînes pour les utiliser plus tard dans votre code.

# Comment extraire des sous-chaînes en TypeScript ?

Pour extraire une sous-chaîne en TypeScript, nous allons utiliser la méthode `substring()` disponible sur les chaînes de caractères. Cette méthode prend deux paramètres : l'index de début et l'index de fin de la sous-chaîne que vous souhaitez extraire. Voici un exemple de code avec une chaîne de caractères et son index :

```TypeScript
let chaine = "Bonjour le monde";
console.log(chaine.substring(0, 6));
```

Ceci va afficher "Bonjour", car nous avons spécifié les index de 0 à 6 pour extraire la première partie de la chaîne. Vous pouvez également utiliser des variables pour spécifier ces index, ce qui peut être pratique dans certaines situations.

```TypeScript
let chaine = "Bonjour le monde";
let debut = 8;
let fin = 12;
console.log(chaine.substring(debut, fin));
```

Cette fois, nous allons obtenir "monde" en tant que sous-chaîne.

# Plongeon en profondeur

La méthode `substring()` en TypeScript utilise les mêmes paramètres que sa version JavaScript. Cela signifie que l'index de début est inclus dans la sous-chaîne, mais pas l'index de fin. Si vous ne spécifiez pas l'index de fin, la méthode va extraire la partie restante de la chaîne à partir de l'index de début. De plus, les index peuvent être négatifs. Dans ce cas, ils comptent à partir de la fin de la chaîne.

Par exemple, si nous utilisons `-8` comme index de début, cela va commencer à partir du huitième caractère à partir de la fin de la chaîne. Si nous utilisons `-4` comme index de fin, cela va s'arrêter à quatre caractères de la fin de la chaîne. Vous pouvez également utiliser des expressions régulières avec la méthode `substring()`, ce qui peut être pratique pour extraire des sous-chaînes basées sur un motif spécifique.

# Voir aussi

- [Documentation officielle TypeScript](https://www.typescriptlang.org/docs)
- [Guide de référence de la méthode `substring()` en TypeScript](https://www.typescriptlang.org/docs/handbook/variable-declarations.html#string-substring)