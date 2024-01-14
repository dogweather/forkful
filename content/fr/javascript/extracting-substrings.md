---
title:    "Javascript: Extraction de sous-chaînes"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est un concept important en programmation Javascript qui peut être utile dans de nombreuses situations. Il s'agit de découper une chaîne de caractères en une partie plus petite en utilisant un index de début et un index de fin. Cela peut être utile pour manipuler, trier ou comparer des chaînes de texte de manière efficace.

## Comment faire

Pour extraire une sous-chaîne en Javascript, nous allons utiliser la méthode `substring()`. Cette méthode prend deux arguments, l'index de début et l'index de fin de notre sous-chaîne. Par exemple, si nous avons une chaîne de caractères "Bonjour", pour extraire la sous-chaîne "jour", nous utiliserons la méthode `substring(3, 6)` car l'index de début pour le caractère "j" est 3 et l'index de fin pour le caractère "r" est 6. Le code suivant illustre cet exemple :

```Javascript
let chaine = "Bonjour";
let sousChaine = chaine.substring(3, 6);

console.log(sousChaine);
// Output : jour
```

Dans cet exemple, nous pouvons également utiliser un index négatif pour extraire la sous-chaîne à partir de la fin de la chaîne de caractères. Par exemple, si nous avons la même chaîne de caractères "Bonjour", pour extraire la sous-chaîne "njour", nous utiliserons la méthode `substring(-3)` car l'index de fin pour le caractère "n" est -3. Le code suivant illustre cet exemple :

```Javascript
let chaine = "Bonjour";
let sousChaine = chaine.substring(-3);

console.log(sousChaine);
// Output : njour
```

Nous pouvons également utiliser la méthode `slice()` pour extraire une sous-chaîne en utilisant des index de début et de fin. La seule différence entre `substring()` et `slice()` est que la méthode `slice()` peut accepter un index négatif pour définir l'index de fin, tandis que `substring()` ne le peut pas. Le code suivant illustre cet exemple :

```Javascript
let chaine = "Bonjour";
let sousChaine = chaine.slice(1, -1);

console.log(sousChaine);
// Output : onjou
```

## Plongée en profondeur

Il est important de noter que la méthode `substring()` ne modifie pas la chaîne de caractères d'origine, elle renvoie simplement la sous-chaîne spécifiée. Si nous voulons modifier la chaîne d'origine, nous devons lui assigner la valeur de la sous-chaîne comme ceci :

```Javascript
let chaine = "Bonjour";
chaine = chaine.substring(1, -1);

console.log(chaine);
// Output : onjou
```

De plus, si nous ne spécifions qu'un seul argument pour la méthode `substring()`, elle utilisera automatiquement la longueur de la chaîne de caractères comme index de fin. Le code suivant illustre cet exemple :

```Javascript
let chaine = "Bonjour";
let sousChaine = chaine.substring(3);

console.log(sousChaine);
// Output : jour
```

## Voir aussi

- [Documentation sur la méthode substring() en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/substring)
- [Documentation sur la méthode slice() en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/slice)