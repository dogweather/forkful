---
title:                "Ruby: Capitalisation d'une chaîne de caractères"
simple_title:         "Capitalisation d'une chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous travaillez avec des chaînes de caractères en Ruby, il peut être utile de les mettre en majuscules pour diverses raisons. Cela peut être pour des questions d'esthétique, pour faciliter la comparaison de chaînes ou pour se conformer à certaines règles de formatage. Dans tous les cas, il est important de savoir comment mettre en majuscules une chaîne de caractères en Ruby. Dans cet article, nous verrons comment le faire de manière efficace.

## Comment procéder

Pour mettre en majuscules une chaîne de caractères en Ruby, nous allons utiliser la méthode `.capitalize`. Voici un exemple de code utilisant cette méthode :

```Ruby
my_string = "bonjour"
puts my_string.capitalize
```

Ce code va produire une sortie `Bonjour`, avec la première lettre en majuscule et toutes les autres en minuscule. Il est important de noter que cette méthode ne modifie pas la chaîne originale, mais renvoie une nouvelle chaîne mise en majuscules.

Vous pouvez également utiliser la méthode `.upcase` pour mettre en majuscules toutes les lettres d'une chaîne, ou la méthode `.downcase` pour les mettre toutes en minuscules. En revanche, la méthode `.capitalize` n'affecte que la première lettre de la chaîne.

## Approfondissement

Il peut être intéressant de comprendre comment la méthode `.capitalize` fonctionne en interne. En fait, elle convertit d'abord la chaîne en un tableau de caractères, met en majuscule le premier élément de ce tableau, puis rejoint les éléments ensemble pour former une nouvelle chaîne. Ceci est important à savoir si vous utilisez d'autres méthodes pour manipuler des chaînes en Ruby, car cela peut avoir un impact sur leur performance.

Il est également important de noter que contrairement à la méthode `.capitalize`, la méthode `.upcase` va également mettre en majuscule les lettres accentuées, tandis que la méthode `.downcase` va les conserver en minuscule.

## Voir aussi

- [La documentation officielle de Ruby sur les méthodes pour modifier des chaînes](https://ruby-doc.org/core-2.7.2/String.html#method-i-capitalize)
- [Un autre article sur la mise en majuscules des chaînes en Ruby (en anglais)](https://www.rubyguides.com/2017/03/ruby-string-methods/)

Merci d'avoir lu cet article sur la mise en majuscules des chaînes en Ruby. J'espère qu'il vous a été utile et que vous pourrez l'appliquer dans vos projets. N'hésitez pas à explorer les autres méthodes pour manipuler les chaînes en Ruby, et à me laisser un commentaire si vous avez des questions ou des suggestions. Bonne programmation !