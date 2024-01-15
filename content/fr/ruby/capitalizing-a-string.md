---
title:                "Capitaliser une chaîne de caractères."
html_title:           "Ruby: Capitaliser une chaîne de caractères."
simple_title:         "Capitaliser une chaîne de caractères."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitale de chaîne est une tâche courante en programmation et peut être utile dans de nombreuses situations. Que ce soit pour mettre en forme des noms ou des titres, ou pour normaliser les données entrées par les utilisateurs, savoir capitaliser une chaîne de caractères peut être un outil précieux dans votre boîte à outils de programmation. Dans cet article, nous allons plonger dans la façon de capitaliser une chaîne en Ruby, pour que vous puissiez l'utiliser dans vos futurs projets.

## Comment faire

Voici un exemple simple de comment capitaliser une chaîne en utilisant la méthode `capitalize` en Ruby :

```Ruby
name = "ruby"
puts name.capitalize
```

Cela produira l'output suivant :

```
Ruby
```

Comme vous pouvez le voir, la méthode `capitalize` a mis en majuscule la première lettre de la chaîne "ruby". Mais qu'en est-il si nous voulons capitaliser toutes les lettres d'une chaîne ? Dans ce cas, nous pouvons utiliser la méthode `upcase` :

```Ruby
name = "ruby"
puts name.upcase
```

Ce qui donne :

```
RUBY
```

Maintenant, si nous voulons capitaliser uniquement la première lettre de chaque mot dans une chaîne, nous pouvons utiliser la méthode `titlecase` de la gem `"titleize"` :

```Ruby
# installation de la gem
gem install "titleize"

require "titleize"

name = "ruby programming"
puts name.titleize
```

L'output sera alors :

```
Ruby Programming
```

Il est également possible d'utiliser des expressions régulières pour capitaliser une chaîne spécifique. Par exemple, si nous voulons capitaliser la première lettre de chaque mot dans une phrase, nous pouvons utiliser la méthode `gsub` avec une expression régulière :

```Ruby
sentence = "i love ruby"
puts sentence.gsub(/\w+/, &:capitalize)
```

Résultat :

```
I Love Ruby
```

## Plongée en profondeur

En utilisant les méthodes `capitalize`, `upcase` et `titleize`, nous pouvons facilement capitaliser une chaîne en Ruby. Cependant, il est important de noter que ces méthodes ne modifient pas la chaîne originale, mais plutôt en créent une nouvelle. Si vous voulez modifier la chaîne d'origine, vous pouvez utiliser le bang `!` à la fin de la méthode, par exemple `capitalize!`.

De plus, il est important de se rappeler que les règles de capitalisation peuvent varier selon la langue et l'utilisation de caractères spéciaux. Dans ce cas, il est recommandé d'utiliser des bibliothèques ou des gems spécifiques à la langue pour une capitalisation précise et adaptée.

## Voir aussi

- [Documentation officielle de la méthode `capitalize` en Ruby](https://ruby-doc.org/core-2.7.2/String.html#method-i-capitalize)
- [Documentation officielle de la méthode `upcase` en Ruby](https://ruby-doc.org/core-2.7.2/String.html#method-i-upcase)
- [Gem "titleize" pour la méthode `titlecase` en Ruby](https://github.com/granth/titleize)