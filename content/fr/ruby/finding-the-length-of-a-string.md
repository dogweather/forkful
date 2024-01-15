---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Ruby: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La longueur d'une chaîne de caractères peut être utile pour beaucoup de choses, comme par exemple vérifier si un mot est valide ou pour la mise en forme d'une chaîne de caractères. Dans cet article, nous allons voir comment trouver la longueur d'une chaîne de caractères en Ruby.

## Comment faire

La méthode `length` peut être utilisée pour trouver la longueur d'une chaîne de caractères en Ruby. Voici un exemple pour trouver la longueur de la chaîne "Bonjour" :

```Ruby
"Bonjour".length
```
Le résultat sera `7`, car "Bonjour" est composé de 7 caractères. Cette méthode fonctionne avec n'importe quelle chaîne de caractères, qu'elle soit vide ou non.

Si vous souhaitez utiliser la longueur d'une chaîne de caractères pour vérifier si elle est égale à un certain nombre, vous pouvez utiliser la méthode `==`, comme ceci :

```Ruby
"Bonjour".length == 7
```

Le résultat sera `true`, car la longueur de "Bonjour" est égale à 7.

## Plongée en profondeur

La méthode `length` compte chaque caractère, même les espaces et les caractères spéciaux. Elle compte également les caractères accentués dans une chaîne de caractères en français.

Il est important de noter que la méthode `length` renvoie un nombre entier représentant la longueur de la chaîne de caractères. Si vous souhaitez utiliser cette valeur dans un calcul mathématique, vous devez convertir le résultat en entier à l'aide de la méthode `to_i`.

## Voir aussi

- [La documentation officielle de Ruby sur la méthode `length`](https://ruby-doc.org/core-2.7.1/String.html#method-i-length)
- [Un tutoriel sur l'utilisation des chaînes de caractères en Ruby](https://www.ruby-lang.org/fr/documentation/quickstart/)