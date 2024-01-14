---
title:    "Javascript: Trouver la longueur d'une chaîne de caractères"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous que la longueur d'une chaîne de caractères est un concept très important en Javascript ? En fait, c'est une tâche courante et très utile lorsque vous travaillez avec des données textuelles. Dans cet article, nous allons explorer en détail comment trouver la longueur d'une chaîne de caractères en Javascript.

## Comment faire

Tout d'abord, il est important de comprendre ce qu'est une chaîne de caractères en Javascript. Il s'agit simplement d'une série de caractères entourés par des guillemets simples ou doubles. Par exemple, "Bonjour" est une chaîne de caractères et sa longueur est de 7, car elle contient 7 lettres.

Maintenant, comment pouvons-nous trouver la longueur d'une chaîne de caractères en Javascript ? Heureusement, il existe une méthode prédéfinie appelée `length` qui peut nous aider. Jetons un coup d'œil à un exemple de code pour mieux comprendre :

```Javascript
let phrase = "Bonjour";
console.log(phrase.length);
```

Dans cet exemple, nous avons créé une variable `phrase` contenant la chaîne de caractères "Bonjour". Ensuite, en utilisant la méthode `length`, nous avons renvoyé la longueur de la chaîne de caractères (ce qui donnera 7 dans ce cas). Il est important de noter que cette méthode renvoie la longueur en comptant chaque caractère, y compris les espaces.

Il est également possible d'utiliser la méthode `length` sur des chaînes de caractères contenues dans des variables de type chaîne de caractères, par exemple :

```Javascript
let nom = "Jean";
let nomLongueur = nom.length;
console.log(nomLongueur);
```

Dans cet exemple, nous avons créé une variable `nom` contenant la chaîne de caractères "Jean". En utilisant la méthode `length`, nous avons stocké la longueur de la chaîne de caractères dans une autre variable appelée `nomLongueur`. Ensuite, en utilisant `console.log`, nous avons imprimé la valeur de la variable `nomLongueur` qui sera de 4 dans ce cas.

## Plongée en profondeur

Maintenant que nous savons comment utiliser la méthode `length` pour trouver la longueur d'une chaîne de caractères, jetons un coup d'œil à quelques informations supplémentaires importantes.

Tout d'abord, il est important de noter que la méthode `length` renvoie un nombre entier. Cela signifie que vous pouvez utiliser cet entier dans des opérations mathématiques ou le comparer à d'autres nombres. Par exemple :

```Javascript
let phrase = "Comment ça va ?";
let longueurPhrase = phrase.length;
if (longueurPhrase > 10) {
    console.log("La phrase est assez longue.");
} else {
    console.log("La phrase est trop courte.");
}
```

Dans cet exemple, nous avons créé une condition qui vérifie si la longueur de la phrase est supérieure à 10. Si c'est le cas, nous imprimons "La phrase est assez longue." Sinon, nous imprimons "La phrase est trop courte."

De plus, la méthode `length` est sensible à la casse. Cela signifie que les majuscules et les minuscules sont comptées différemment. Par exemple, si nous utilisons `length` sur les chaînes de caractères "Bonjour" et "bonjour", nous obtiendrons des longueurs différentes (respectivement 7 et 6).

Enfin, il est important de noter que la méthode `length` renverra également une longueur pour d'autres types de données, tels que des tableaux ou des objets. Dans ces cas, elle renverra le nombre d'éléments dans le tableau ou le nombre de propriétés de l'objet.

## Voir aussi

Pour en savoir plus sur la méthode `length` et comment l'utiliser en Javascript, vous pouvez consulter les ressources suivantes :

- [Documentation Mozilla sur la propriété `length`](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Article sur les chaînes de caractères en Javascript](https://www.grafikart.fr/tutoriels/javascript/chaine-caractere-javascript)
- [Exemples pratiques de l'utilisation de la méthode `length`](https://www.w3schools.com/jsref/jsref_length_string.asp