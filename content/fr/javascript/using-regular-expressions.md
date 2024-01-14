---
title:                "Javascript: Utiliser des expressions régulières"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Pourquoi utiliser les expressions régulières en Javascript ?

Les expressions régulières sont des outils puissants qui permettent de rechercher et de manipuler des chaînes de caractères en utilisant des motifs spécifiques. En Javascript, les expressions régulières sont largement utilisées pour valider des entrées utilisateur, effectuer des recherches et des remplacements dans des textes, et bien plus encore.

# Comment utiliser les expressions régulières en Javascript

Pour utiliser les expressions régulières en Javascript, il faut d'abord créer une instance de l'objet `RegExp` à l'aide de la notation littérale `/pattern/` ou en appelant le constructeur `RegExp()`. Par exemple :

```Javascript
let str = "Les expressions régulières sont très utiles.";
let regex = /régulières/;
```

Ensuite, on peut utiliser différentes méthodes telles que `test()`, `exec()` et `match()` pour effectuer des opérations avec l'expression régulière sur la chaîne de caractères. Voici un exemple de code montrant l'utilisation de ces méthodes :

```Javascript
let str = "Les expressions régulières sont très utiles.";
let regex = /régulières/;
regex.test(str); // Retourne true
regex.exec(str); // Retourne une array avec le mot "régulières"
str.match(regex); // Retourne également une array avec le mot "régulières"
```

Les expressions régulières peuvent également être combinées avec des modificateurs pour rendre la recherche plus spécifique. Par exemple, en utilisant le modificateur `i`, la recherche sera insensible à la casse, c'est-à-dire qu'elle trouvera des correspondances pour "régulières" et "RÉGULIÈRES". On peut également utiliser des caractères spéciaux pour définir des motifs plus complexes, tels que `[a-z]` pour trouver n'importe quelle lettre minuscule.

# Plongée plus profonde dans les expressions régulières en Javascript

Les expressions régulières peuvent paraître intimidantes au début, mais elles deviennent rapidement un outil précieux une fois que l'on comprend leur fonctionnement. En plus des méthodes mentionnées précédemment, il existe d'autres méthodes telles que `replace()` et `split()` qui peuvent être utilisées avec des expressions régulières pour manipuler des chaînes de caractères.

De plus, les expressions régulières peuvent être utilisées pour valider des formats spécifiques tels que les adresses e-mail, les numéros de téléphone, etc. en utilisant des motifs prédéfinis ou en créant ses propres motifs.

Il peut être utile de pratiquer et de se familiariser avec les expressions régulières en utilisant des sites tels que [Regex101](https://regex101.com/) ou [RegExr](https://regexr.com/), qui offrent des outils pour tester et expérimenter avec des expressions régulières en temps réel.

# Voir aussi

- [Documentation officielle de MDN sur les expressions régulières en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Tutoriel sur les expressions régulières en JavaScript de RegexOne](https://regexone.com/lesson/javascript)
- [Vidéo sur les expressions régulières en Javascript par Traversy Media](https://www.youtube.com/watch?v=rhzKDrUiJVk)