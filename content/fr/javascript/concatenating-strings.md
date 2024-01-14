---
title:                "Javascript: La concaténation de chaînes de caractères"
simple_title:         "La concaténation de chaînes de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

En programmation Javascript, il est souvent nécessaire de combiner plusieurs chaînes de caractères en une seule. Cela peut être utile pour créer des phrases dynamiques ou pour manipuler des données. La concaténation de chaînes est donc une compétence importante à maîtriser pour les développeurs.

## Comment faire

Pour concaténer des chaînes en Javascript, nous utilisons l'opérateur "+" ou la méthode "concat". Voici un exemple de code :

```Javascript
let nom = "Marie";
let age = 28;
let message = "Je m'appelle " + nom + " et j'ai " + age + " ans.";
console.log(message);
```

Le code ci-dessus affichera "Je m'appelle Marie et j'ai 28 ans." dans la console.

Une autre façon de concaténer des chaînes est d'utiliser la méthode "concat" :

```Javascript
let nom = "Pierre";
let age = 35;
let message = "Je m'appelle ".concat(nom, " et j'ai ", age , " ans.");
console.log(message);
```

Cette fois-ci, le même résultat sera affiché dans la console. La seule différence est que la méthode "concat" nous permet de concaténer plusieurs chaînes sans utiliser d'opérateur "+".

## Plongée en profondeur

Il est important de noter que la concaténation de chaînes peut également se faire avec d'autres types de données, tels que des nombres ou des variables booléennes. Dans ce cas, Javascript convertira ces données en chaînes avant de les concaténer.

Il est également possible de concaténer des chaînes en utilisant des templates de chaînes (ou templates littéraux). Cela permet d'insérer facilement des variables dans une chaîne sans avoir à utiliser l'opérateur "+" ou la méthode "concat". Voici un exemple :

```Javascript
let nom = "Sophie";
let age = 42;
let message = `Je m'appelle ${nom} et j'ai ${age} ans.`;
console.log(message);
```

Ce code produira le même résultat que les deux exemples précédents.

## Voir aussi

- [MDN web docs - Opérateur de concaténation](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Op%C3%A9rateurs/Op%C3%A9rateurs_de_concat%C3%A9nation)
- [MDN web docs - Méthode concat](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/concat)
- [MDN web docs - Template literals](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Litt%C3%A9raux_gabarits)