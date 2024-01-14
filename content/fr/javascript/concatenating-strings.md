---
title:    "Javascript: Concaténation de chaînes de caractères."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà eu besoin de fusionner différentes chaînes de caractères en une seule ? Peut-être pour créer un message personnalisé, une URL ou même pour afficher des données dans votre code ? C'est là que la concaténation de chaînes de caractères entre en jeu.

## Comment faire

Pour concaténer des chaînes de caractères en Javascript, il suffit d'utiliser l'opérateur `+` pour les assembler. Voyons cela en action avec un exemple de code :

```
let nom = "Marie";
let age = 25;
let message = nom + " a " + age + " ans.";
console.log(message);
```

Dans cet exemple, nous avons défini une variable `nom` avec la valeur "Marie" et une variable `age` avec la valeur 25. Ensuite, nous avons utilisé l'opérateur `+` pour concaténer ces variables et créer une nouvelle variable `message` contenant le message "Marie a 25 ans.". Enfin, en utilisant la fonction `console.log()`, nous pouvons afficher ce message dans la console.

En plus de l'opérateur `+`, vous pouvez également utiliser la méthode `concat()` pour concaténer des chaînes de caractères. Par exemple :

```
let prenom = "Jean";
let nom = "Dupont";
let message = prenom.concat(" ", nom);
console.log(message);
```

Cela produira le même résultat que l'exemple précédent, affichant "Jean Dupont" dans la console.

## Plongée en profondeur

Il est important de noter que la concaténation de chaînes de caractères peut également être utilisée pour convertir d'autres types de données en chaînes. Par exemple, si vous avez besoin de concaténer une chaîne de caractères avec un nombre, vous devrez d'abord le convertir en chaîne à l'aide de la méthode `toString()`. Par exemple :

```
let age = 30;
let message = "Marie a " + age.toString() + " ans.";
console.log(message);
```

Cela produira le résultat attendu, "Marie a 30 ans.". En utilisant `toString()` sur une variable, nous pouvons obtenir la représentation sous forme de chaîne de caractères de cette variable à utiliser dans la concaténation.

## Voir aussi

- [Documentation MDN sur la concaténation de chaînes de caractères en Javascript (en anglais)](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Article sur la conversion de types en Javascript (en français)](https://www.grafikart.fr/tutoriels/meteor/node-notion-types-conversion-868)
- [Vidéo sur la concaténation de chaînes de caractères en Javascript (en français)](https://www.youtube.com/watch?v=c2oRuYX3Hi8)