---
title:                "Ruby: Convertir une chaîne en minuscules"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi:

La conversion en minuscules est un processus courant en programmation de Ruby. Cela peut être utile si vous devez comparer des chaînes de caractères sans tenir compte de la casse, ou si vous voulez simplement uniformiser le format de vos données. Dans cet article, nous allons explorer les différentes façons de convertir une chaîne en minuscules en utilisant Ruby.

## Comment faire:

Nous pouvons utiliser la méthode `downcase` pour convertir une chaîne en minuscules. Voici un exemple de code:

```ruby
phrase = "JE SUIS EN MAJUSCULES."

puts phrase.downcase
```

**Sortie:**
je suis en majuscules.

Nous pouvons également utiliser la méthode `downcase!` pour modifier la chaîne d'origine sans avoir à en créer une nouvelle. Voici un exemple:

```ruby
phrase = "JE SUIS EN MAJUSCULES."

phrase.downcase!
puts phrase
```

**Sortie:**
je suis en majuscules.

Il est également possible d'utiliser la méthode `tr` pour convertir une chaîne en modifiant chaque lettre individuellement. Voici un exemple:

```ruby
phrase = "JE SUIS EN MAJUSCULES."

puts phrase.tr('A-Z', 'a-z')
```

**Sortie:**
je suis en majuscules.

## Plongée profonde:

Il y a quelques points à considérer lors de la conversion d'une chaîne en minuscules en utilisant Ruby. Tout d'abord, il est important de noter que la méthode `downcase` ne modifiera pas une chaîne si elle ne contient aucun caractère en majuscule. Par exemple:

```ruby
phrase = "je suis en minuscules."

puts phrase.downcase
```

**Sortie:**
je suis en minuscules.

De plus, la méthode `downcase` ne prend pas en compte les caractères spéciaux, tels que les accents ou les caractères non alphabétiques, lors de la conversion en minuscules. Par exemple:

```ruby
phrase = "Être ou ne pas être."

puts phrase.downcase
```

**Sortie:**
être ou ne pas être.

Enfin, il existe également des méthodes similaires pour convertir une chaîne en majuscules (`upcase` et `upcase!`) et pour modifier le premier caractère en majuscule (`capitalize` et `capitalize!`).

## Voir aussi:

- La documentation officielle de Ruby sur la méthode `downcase`: https://ruby-doc.org/core-2.6/String.html#method-i-downcase
- Un guide sur les méthodes de manipulation de chaînes en Ruby: https://www.rubyguides.com/2015/09/ruby-string-methods/
- Les différences entre `downcase` et `tr` pour la conversion en minuscules: https://stackoverflow.com/questions/18561646/difference-between-downcasetr-and-tr-in-ruby