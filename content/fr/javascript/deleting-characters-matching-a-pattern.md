---
title:    "Javascript: Suppression des caractères correspondant à un motif"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un modèle peut être utile dans de nombreuses situations de programmation. Par exemple, si vous travaillez avec une chaîne de caractères et que vous souhaitez supprimer tous les espaces blancs, utiliser cette méthode vous permettra d'obtenir une chaîne plus propre et plus facile à manipuler.

## Comment faire

Pour supprimer des caractères correspondant à un modèle en Javascript, vous pouvez utiliser la méthode `replace()` en utilisant une expression régulière. Voici un exemple de code pour supprimer tous les espaces blancs d'une chaîne de caractères :

```Javascript
let str = "Ceci est un exemple de chaîne de caractères";
let newStr = str.replace(/ /g, "");
console.log(newStr);
```

Le résultat de ce code sera `Ceciestunexempledechainedecaracteres`, sans aucun espace blanc.

## Approfondissement

La méthode `replace()` utilise des expressions régulières, également appelées "regex", pour trouver et remplacer des caractères correspondant à un modèle donné. Vous pouvez utiliser une variété d'expressions régulières pour trouver différents modèles de caractères, tels que des lettres majuscules, des chiffres ou même des caractères spéciaux.

Par exemple, pour supprimer tous les chiffres d'une chaîne de caractères, vous pouvez utiliser l'expression régulière `/[0-9]/g`, qui trouvera tous les chiffres et les remplacera par une chaîne vide. Il est important de noter que l'utilisation de l'option "g" à la fin de l'expression régulière indique que la recherche doit être effectuée globalement, ce qui signifie qu'elle continuera de supprimer toutes les occurrences correspondantes dans la chaîne.

De plus, vous pouvez utiliser différentes méthodes en combinaison avec `replace()` pour manipuler les chaînes de caractères de différentes manières. Par exemple, vous pouvez supprimer tous les caractères à l'exception des lettres en utilisant l'expression régulière `/[^a-zA-Z]/g` et en les remplaçant par une chaîne vide.

## Voir aussi

Pour en savoir plus sur l'utilisation des expressions régulières en Javascript pour supprimer des caractères correspondant à un motif, vous pouvez consulter les ressources suivantes :

- [MDN Web Docs - Méthode replace()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- [RegexOne - Tutoriels sur les expressions régulières](https://regexone.com/)
- [W3Schools - Exemples de regex en Javascript](https://www.w3schools.com/js/js_regexp.asp)