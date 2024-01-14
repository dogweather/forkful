---
title:                "Ruby: Capitaliser une chaîne de caractères"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Nous avons tous rencontré des situations où nous avons besoin de changer la casse d'une chaîne de caractères. Que ce soit pour rendre un texte plus lisible ou pour suivre une convention de nommage, la capitalisation des chaînes de caractères est un problème courant dans la programmation.

## Comment faire

Pour capitaliser une chaîne de caractères en Ruby, il suffit d'utiliser la méthode `capitalize` :

```Ruby
"bonjour".capitalize  #=> "Bonjour"
```

Si vous souhaitez capitaliser chaque mot d'une phrase, vous pouvez utiliser la méthode `titleize` :

```Ruby
"je suis un programmeur en ruby".titleize  #=> "Je Suis Un Programmeur En Ruby"
```

Il est également possible de capitaliser uniquement la première lettre de chaque mot, en utilisant la méthode `swapcase` :

```Ruby
"banane est un fruit".swapcase  #=> "Banane Est Un Fruit"
```

## Plongée en profondeur

Maintenant que nous avons vu comment capitaliser une chaîne de caractères en Ruby, il est intéressant de comprendre comment cela fonctionne en interne. En réalité, Ruby utilise la méthode `capitalize` pour effectuer cette tâche. Cette méthode modifie simplement la première lettre de la chaîne en majuscule et laisse le reste de la chaîne inchangé. Cependant, la méthode `titleize` utilise des règles de capitalisation plus complexes pour les langues telles que l'anglais, qui capitalise également la première lettre après un symbole de ponctuation et convertit les acronymes en majuscules.

## Voir aussi

- [Documentation officielle de Ruby sur la méthode `capitalize`](https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize)
- [Documentation officielle de Ruby sur la méthode `titleize`](https://apidock.com/rails/String/titleize)
- [Documentation officielle de Ruby sur la méthode `swapcase`](https://ruby-doc.org/core-2.7.1/String.html#method-i-swapcase)