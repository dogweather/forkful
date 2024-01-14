---
title:    "Javascript: Extraction de sous-chaïnes"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes novice en programmation, vous vous demandez peut-être pourquoi quelqu'un voudrait extraire des sous-chaînes. En réalité, c'est une capacité très utile lorsqu'il s'agit de manipuler des chaînes de caractères. Que ce soit pour traiter des données complexes ou simplement pour organiser du texte, la fonction d'extraction de sous-chaînes peut vous faire gagner un temps précieux.

## Comment faire

Pour extraire une sous-chaîne en Javascript, vous devez utiliser la méthode `substring()`, qui prend deux paramètres: l'index de début et l'index de fin de la sous-chaîne que vous souhaitez extraire. Le code suivant illustre son utilisation:

```Javascript
let str = "Bonjour le monde!";
let subStr = str.substring(10, 16);
console.log(subStr); // sortie: monde!
```

Dans cet exemple, nous avons extrait la sous-chaîne "monde!" en utilisant les index 10 (lettre "m") et 16 (le caractère "!" est exclu de la sous-chaîne).

Vous pouvez également utiliser la méthode `slice()`, qui a la même syntaxe que `substring()` mais qui prend les index négatifs en compte. Cela signifie que vous pouvez extraire une sous-chaîne à partir de la fin de la chaîne de caractères. Voici un exemple pour mieux comprendre:

```Javascript
let str = "Bonjour le monde!";
let subStr = str.slice(-6);
console.log(subStr); // sortie: monde!
```

Ici, nous avons extrait la sous-chaîne "monde!" en utilisant un index négatif, ce qui signifie qu'elle commence à compter à partir de la fin de la chaîne. L'utilisation des index négatifs est particulièrement utile lorsque vous faites face à des chaînes de caractères de longueur variable.

Enfin, il est important de noter que la méthode `substring()` et `slice()` ne modifient pas la chaîne d'origine, elles renvoient simplement une nouvelle sous-chaîne.

## Plongée en profondeur

Bien que la méthode `substring()` et `slice()` soient très utiles, elles ont des différences importantes à prendre en compte lors de leur utilisation. Voici quelques-unes des différences les plus importantes:

- `substring()` ne prend pas les index négatifs en compte, alors que `slice()` oui.
- Si l'index de fin est omis, `substring()` prendra tous les caractères de l'index de début jusqu'à la fin de la chaîne, tandis que `slice()` renvoie juste une chaîne vide.
- Si l'index de début est supérieur à l'index de fin, `substring()` les échangera automatiquement, tandis que `slice()` renverra une chaîne vide.

En connaissant ces différences, vous serez en mesure de choisir la méthode la plus appropriée pour votre situation.

## Voir aussi

- [Documentation MDN sur la méthode `substring()` en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/substring)
- [Documentation MDN sur la méthode `slice()` en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/slice)
- [Un tutoriel vidéo sur l'extraction de sous-chaînes en Javascript](https://www.youtube.com/watch?v=Pksedas2aJo)