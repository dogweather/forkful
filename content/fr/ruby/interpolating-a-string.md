---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolation de chaîne en Ruby

## Qu'est-ce que c'est et pourquoi ?

L'interpolation de chaînes en Ruby est un moyen de remplacer une variable ou une expression dans une chaîne de caractères. Elle permet de simplifier l'écriture et la lecture du code.

## Comment faire:

```Ruby
nom = 'Alice'

# Voici comment faire une interpolation de chaîne en Ruby
puts "Bonjour #{nom}"
```

Cela affiche:

```
Bonjour Alice
```

## Plongée en profondeur:

Historiquement, l'interpolation de chaînes existait avant même Ruby, dans des langues comme Perl et Shell scripting. En Ruby, nous avons également des alternatives comme l'utilisation de la méthode `+` ou `concat` pour concaténer des chaînes. Cependant, l'interpolation est plus efficace et plus lisible.

Détails d'implémentation - Ruby reconnaît l'interpolation de chaîne en utilisant des guillemets doubles `" "`. Dans les guillemets simples `' '`, Ruby ne fera pas d'interpolation.

## Voir aussi:

- Documentation Ruby sur l'interpolation de chaînes: https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-Strings
- Guide d'utilisation des chaînes en Ruby: https://www.rubyguides.com/2018/01/ruby-string-methods/
- Comparaison de l'interpolation et de la concaténation de chaînes: http://ruby-for-beginners.rubymonstas.org/bonus/string_interpolation_vs_concatenation.html.