---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

La concaténation de chaînes est l'action de joindre deux ou plusieurs chaînes de caractères en une seule. Les programmeurs le font pour manipuler et afficher du texte de manière efficace et pratique.

## Comment faire:

En Ruby, il y a plusieurs façons de concaténer des chaînes. On peut utiliser l'opérateur `+` ou la méthode `.concat`. Je vais montrer les deux façons.

```Ruby
 # Utiliser l'opérateur +
chaine1 = "Bonjour "
chaine2 = "monde!"
resultat = chaine1 + chaine2
puts resultat
#sortie: "Bonjour monde!"
```

```Ruby
 # Utiliser la méthode .concat
chaine1 = "Bonjour "
chaine2 = "monde!"
chaine1.concat(chaine2)
puts chaine1
#sortie: "Bonjour monde!"
```

## Plongeon Profond:

Historiquement, la concaténation de chaînes est une technique assez ancienne, présente dans de nombreux langages de programmation.

En ce qui concerne les alternatives, outre l'opérateur `+` et la méthode `.concat`, en Ruby, il existe l'interpolation de chaînes. Vous pouvez intégrer une variable directement dans une chaîne avec #{}.

```Ruby
chaine1 = "monde"
puts "Bonjour #{chaine1}!"
#sortie: "Bonjour monde!"
```

Cependant, notez que les différentes techniques de concaténation peuvent avoir un impact sur les performances de votre code. En général, l'interpolation est plus rapide que la concaténation avec l'opérateur `+` dans Ruby.

(N'oubliez pas que l'opérateur `+` crée une nouvelle chaîne alors que `.concat` et l'interpolation modifient la chaîne d'origine.)

## Voir Aussi:

Pour plus d'informations sur la concaténation des chaînes et d'autres techniques associées à Ruby, consultez ces liens utiles:

1. [La Documentation Officielle de Ruby](https://ruby-doc.org/core-2.7.0/String.html)
2. [Le Guide de Ruby sur les Chaînes de Caractères](http://rubylearning.com/satishtalim/ruby_strings.html)
3. [Discussion Stackoverflow sur la Concaténation de Chaînes](https://stackoverflow.com/questions/4684446/why-is-string-concatenation-faster-than-array-join)