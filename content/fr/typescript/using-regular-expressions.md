---
title:                "Utiliser les expressions régulières"
html_title:           "TypeScript: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Introduction

Les expressions régulières sont un outil puissant utilisé par les programmeurs pour rechercher et manipuler du texte dans des chaînes de caractères. Que vous soyez nouveau dans le monde de la programmation ou que vous soyez un développeur expérimenté, vous apprendrez à quel point les expressions régulières peuvent être utiles dans vos projets.

## Quoi et pourquoi ?

Les expressions régulières sont des motifs ou des séquences de caractères qui sont utilisées pour rechercher et manipuler du texte dans une chaîne de caractères. Les programmeurs les utilisent pour valider des données telles que des adresses e-mail, des numéros de téléphone ou des mots de passe, ainsi que pour effectuer des tâches telles que la recherche et le remplacement de texte. Elles permettent également de gagner du temps lors de la manipulation de grandes quantités de données.

## Comment faire :

```TypeScript
const regex = /hello/; // Crée une expression régulière pour trouver le mot "hello"
const str = 'Bonjour tout le monde!';

console.log(regex.test(str)); // Sortie : false, car "hello" ne se trouve pas dans la variable str
console.log(str.replace(regex, 'hello')); // Sortie : "Bonjour tout le monde!", car aucun mot "hello" n'a été trouvé pour être remplacé
```

Dans l'exemple ci-dessus, nous avons déclaré une expression régulière en utilisant le "/" comme délimiteur et en plaçant le mot "hello" entre les deux. La méthode "test" vérifie si le mot "hello" se trouve dans la variable "str". La méthode "replace" recherche le mot "hello" dans la variable "str" et le remplace par "hello".

## Plongée en profondeur :

Les expressions régulières existent depuis plus de 50 ans, vu pour la première fois dans les années 1950 par les mathématiciens Stephen Cole Kleene et Leonhard Euler. Les alternatives aux expressions régulières incluent l'utilisation de fonctions de chaînes, mais elles ne sont pas aussi flexibles et puissantes que les expressions régulières. En TypeScript, les expressions régulières sont mises en œuvre en utilisant la classe "RegExp" et supportent les mêmes spécifications que JavaScript.

## À voir aussi :

- [Documentation sur les expressions régulières en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Expressions_r%C3%A9guli%C3%A8res)