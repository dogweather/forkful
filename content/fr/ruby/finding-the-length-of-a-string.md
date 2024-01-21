---
title:                "Trouver la longueur d'une chaîne de caractères"
date:                  2024-01-20T17:48:12.431292-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?

Trouver la longueur d'une chaîne de caractères, c'est simplement compter le nombre de caractères qu'elle contient. Les programmeurs le font pour valider des entrées, manipuler des textes ou pour la logique de certains algorithmes.

## Comment faire :

```ruby
# Exemple simple
chaine = "Salut les codeurs!"
puts chaine.length
# => 18

# Compter les caractères dans une phrase
phrase = "La programmation en Ruby, c'est cool."
puts phrase.length
# => 37

# Fonctionne aussi avec les caractères spéciaux et espaces
special = "Ça compte aussi les espaces !"
puts special.length
# => 29
```

## Exploration approfondie

Avant, en Ruby, on utilisait souvent `.size` qui est synonyme de `.length`. Les deux méthodes sont interchangeables, mais `.length` est plus explicite pour les chaînes de caractères. Par ailleurs, sous le capot, Ruby maintient un compteur pour chaque chaîne, donc appeler `.length` est une opération en temps constant, très rapide.

Dans certains langages, il faut parcourir toute la chaîne pour connaître sa taille. Mais Ruby, comme beaucoup d'autres langages modernes, garde cette information accessible en un clin d'œil.

Une alternative existe : `String#bytesize`. Elle compte le nombre d'octets, ce qui peut être différent du nombre de caractères, surtout si la chaîne contient des caractères encodés sur plusieurs octets (comme certains caractères accentués ou émojis).

## À voir également

- [Ruby API doc sur les méthodes String](https://www.rubydoc.info/stdlib/core/String)
- [Article sur l’encodage en Ruby](https://www.justinweiss.com/articles/3-steps-to-fix-encoding-problems-in-ruby/)