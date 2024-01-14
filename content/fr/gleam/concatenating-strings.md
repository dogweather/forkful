---
title:                "Gleam: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une pratique courante en programmation qui consiste à joindre plusieurs chaînes de caractères ensemble pour créer une nouvelle chaîne. Cette technique est souvent utilisée pour créer des messages personnalisés, des rapports ou des requêtes pour les bases de données. Que vous soyez un programmeur débutant ou expérimenté, la concaténation de chaînes de caractères est une compétence clé à maîtriser dans n'importe quel langage de programmation.

## Comment faire

Pour concaténer des chaînes de caractères en Gleam, nous utilisons l'opérateur "+" qui combine deux chaînes de caractères ensemble. Par exemple, si nous voulons créer une nouvelle chaîne en concaténant les mots "Bonjour" et "monde", nous pouvons écrire :

```Gleam
let message = "Bonjour" + "monde"
```

La variable "message" contiendra maintenant la chaîne "Bonjourmonde". Nous pouvons également concaténer des variables contenant des chaînes de caractères pour créer des messages personnalisés. Voici un exemple :

```Gleam
let nom = "Alice"
let message = "Bonjour " + nom
```

La variable "message" contiendra maintenant la chaîne "Bonjour Alice". En utilisant cette méthode, vous pouvez créer des chaînes de caractères qui répondent à des besoins spécifiques dans vos programmes.

## Plongée en profondeur

En Gleam, la concaténation de chaînes de caractères est une opération efficace car elle est optimisée au niveau du compilateur. Cela signifie que le compilateur Gleam peut prédire à l'avance la longueur de la chaîne concaténée et allouer l'espace mémoire nécessaire. Cela rend l'opération plus rapide qu'en utilisant d'autres méthodes telles que la concaténation avec "++" ou en utilisant des boucles.

Cependant, il est important de noter que la concaténation de chaînes de caractères peut rapidement devenir inefficace si elle est utilisée à grande échelle ou si elle est combinée avec d'autres opérations. Dans ces cas, il est conseillé d'utiliser des structures de données plus performantes telles que les tableaux de chaînes de caractères ("string arrays") ou les tampons de chaînes de caractères ("string buffers"). En règle générale, il est préférable d'utiliser la concaténation de chaînes de caractères pour des opérations simples ou avec un petit nombre de chaînes de caractères.

## Voir aussi

- [Documentation officielle de la concaténation de chaînes de caractères en Gleam](https://gleam.run/book/core/string.html#string-concatenation)
- [Autres opérations sur les chaînes de caractères en Gleam](https://gleam.run/book/core/string.html)
- [Guide pour optimiser les opérations sur les chaînes de caractères en Gleam](https://gleam.run/book/optimisation/strings.html)