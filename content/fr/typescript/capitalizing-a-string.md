---
title:                "Capitalisation d'une chaîne de caractères"
html_title:           "TypeScript: Capitalisation d'une chaîne de caractères"
simple_title:         "Capitalisation d'une chaîne de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation de chaînes de caractères peut être utile lors de la manipulation de données afin de les mettre en forme de manière cohérente ou pour répondre à des exigences de présentation. Elle peut également être utilisée pour rendre les chaînes de caractères plus lisibles pour les utilisateurs finaux.

## Comment faire

```TypeScript
function capitalize(str: string): string {
  // Vérifie si la chaîne est vide ou nulle
  if (!str) {
    return "";
  }
  // Divise la chaîne en mots séparés par des espaces
  const words = str.split(" ");
  // Parcours chaque mot et met en majuscule la première lettre
  for (let i = 0; i < words.length; i++) {
    words[i] = words[i][0].toUpperCase() + words[i].slice(1);
  }
  // Reconstruit la chaîne avec les mots capitalisés
  return words.join(" ");
}

console.log(capitalize("bonjour le monde")); // Bonjour Le Monde
console.log(capitalize("javascript est super")); // Javascript Est Super
console.log(capitalize("")); // (chaîne vide)
```

## Analyse approfondie

La méthode `split` utilisée dans l'exemple divise une chaîne en un tableau de sous-chaînes en utilisant un séparateur. Dans notre cas, le séparateur est un espace, donc chaque mot est stocké dans une case du tableau. Ensuite, une boucle `for` parcourt chaque mot et utilise la méthode `toUpperCase` pour mettre en majuscule la première lettre. La méthode `slice` est également utilisée pour récupérer le reste du mot à partir de la deuxième lettre. Enfin, la méthode `join` est utilisée pour reconstruire la chaîne avec les mots capitalisés.

## Voir aussi

- [La méthode `split` en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/split)
- [La méthode `toUpperCase` en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toUpperCase)
- [La méthode `slice` en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/slice)
- [La méthode `join` en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Array/join)