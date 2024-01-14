---
title:                "TypeScript: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil extrêmement utile pour tout programmeur TypeScript. Elles permettent de rechercher des motifs spécifiques dans du texte et de manipuler ou extraire ces données de manière efficace. Cela peut être utile pour la validation de données utilisateur, la vérification de formats de texte, et bien plus encore.

## Comment Faire

Les expressions régulières sont créées en utilisant le constructeur `RegExp`. Voici un exemple de code TypeScript montrant comment créer une expression régulière qui recherche les adresses e-mail dans une chaîne de texte :

```TypeScript
const emailRegex = new RegExp(/(\w+@[a-z]+\.[a-z]+)/);

const text = "Mon adresse e-mail est example@email.com.";

const results = emailRegex.exec(text);
console.log(results);

// ['example@email.com', index: 22, input: 'Mon adresse e-mail est example@email.com.']
```

Dans cet exemple, nous utilisons le constructeur `RegExp` pour créer une expression régulière qui recherche une adresse e-mail valide (`\w+` représente une suite de caractères alphanumériques, `@` représente le symbole "@" et `[a-z]+` représente une suite de caractères alphabétiques). Nous utilisons ensuite la méthode `exec` pour appliquer notre expression à la chaîne de texte et obtenir un tableau des résultats.

Il existe de nombreux autres modificateurs et caractères spéciaux que vous pouvez utiliser dans vos expressions régulières, comme les quantificateurs `+`, `*` et `?` pour spécifier des correspondances multiples ou facultatives. Vous pouvez également utiliser des parenthèses pour créer des groupes de correspondance et extraire des portions spécifiques de la chaîne. Il existe également des raccourcis pour des motifs couramment utilisés, tels que `/\d/` pour correspondre à un chiffre.

## Plongée Profonde

Les expressions régulières peuvent sembler intimidantes au premier abord, mais elles sont très flexibles et puissantes une fois que vous les maîtrisez. Il est important de bien comprendre les différents modificateurs et caractères spéciaux afin de créer des expressions efficaces et précises. Il peut également être utile de tester vos expressions régulières sur des sites en ligne tels que [Regex101](https://regex101.com/) pour voir comment elles fonctionnent sur différents types de données.

## Voir Aussi

- [Documentation officielle de TypeScript sur les expressions régulières](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Un guide complet pour les expressions régulières en JavaScript](https://javascript.info/regular-expressions)
- [Vidéo explicative sur les expressions régulières en TypeScript](https://www.youtube.com/watch?v=dyUxZeBFedM)