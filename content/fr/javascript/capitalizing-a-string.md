---
title:                "Majusculation d'une chaîne de caractères."
html_title:           "Javascript: Majusculation d'une chaîne de caractères."
simple_title:         "Majusculation d'une chaîne de caractères."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être déjà rencontré une situation où vous deviez capitaliser une chaîne de caractères en Javascript. Peut-être que vous traitiez des données utilisateur ou que vous aviez besoin d'afficher une phrase correctement dans votre application. Dans ces cas-là, il est utile de savoir comment capitaliser une chaîne en Javascript.

## Comment Faire

Capitaliser une chaîne de caractères en Javascript peut être fait de plusieurs manières, en utilisant différentes méthodes. Voici quelques exemples de code pour vous montrer comment le faire :

```Javascript
// Méthode 1 : Utilisation de la méthode toUpperCase()
let string = "exEmPLe";
string = string.toUpperCase();
// Output : "EXEMPLE"

// Méthode 2 : Utilisation de la méthode capitalize()
String.prototype.capitalize = function() {
  return this.charAt(0).toUpperCase() + this.slice(1);
}
let string = "exEmPLe";
string = string.capitalize();
// Output : "ExEmPLe"

// Méthode 3 : Utilisation de la méthode replace()
let string = "exEmPLe";
string = string.replace(/^\w/, c => c.toUpperCase());
// Output : "ExEmPLe"
```

Vous pouvez également utiliser une combinaison de ces méthodes ou en créer une qui répond à vos besoins spécifiques. L'essentiel est de comprendre comment le processus de capitalisation fonctionne et de trouver la méthode ou la combinaison de méthodes qui fonctionne le mieux pour vous.

## Plongée Profonde

En Javascript, les chaînes de caractères sont des objets et peuvent donc avoir des méthodes et des propriétés. La méthode `toUpperCase()` que nous avons utilisée dans le premier exemple est une méthode intégrée qui convertit tous les caractères d'une chaîne en majuscules.

La deuxième méthode, `capitalize()`, est une méthode que nous avons créée en l'ajoutant au prototype de l'objet String. Cela signifie que cette méthode sera disponible pour toutes les chaînes de caractères que nous créons. Elle utilise la méthode `charAt()` pour obtenir le premier caractère de la chaîne et la méthode `slice()` pour obtenir le reste de la chaîne à partir du deuxième caractère. Ensuite, elle utilise la méthode `toUpperCase()` pour convertir la première lettre en majuscule et concatène le reste de la chaîne pour obtenir la chaîne capitalisée complète.

Enfin, la troisième méthode utilise la méthode `replace()` pour remplacer le premier caractère de la chaîne en utilisant une expression régulière et la fonction fléchée ES6 pour convertir ce premier caractère en majuscule.

Il est important de noter qu'en Javascript, les chaînes de caractères sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées directement. C'est pourquoi nous avons assigné les résultats de chaque méthode à une nouvelle variable dans les exemples ci-dessus.

## Voir Aussi

- Documentation MDN sur la méthode `toLowerCase()`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toLowerCase
- Tutoriel Codecademy sur les méthodes de manipulation de chaînes en Javascript: https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-strings/cheatsheet