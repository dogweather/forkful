---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Quoi et Pourquoi ?

Capitaliser une chaîne de caractères, c'est transformer sa première lettre en majuscule. On le fait pour respecter les règles de grammaire, par exemple, au début des phrases ou pour les noms propres.

## How to / Comment faire :

En Ruby, plusieurs méthodes permettent de capitaliser une chaîne. Voici les plus courantes :

```Ruby
# Utilisation de la méthode `capitalize`
prenom = "jean"
prenom_capitalise = prenom.capitalize
puts prenom_capitalise
# => Jean

# Capitaliser toutes les initiales avec `split` et `map`
titre = "le seigneur des anneaux"
titre_maj = titre.split.map(&:capitalize).join(' ')
puts titre_maj
# => Le Seigneur Des Anneaux
```

## Deep Dive / Plongée en Profondeur :

Historiquement, la méthode `capitalize` et d'autres méthodes de traitement des chaînes viennent de langages plus anciens comme Perl. Ruby les a adoptées et simplifiées.

Il y a des alternatives si on veut plus de contrôle, comme utiliser des expressions régulières (RegEx).

Pour le fonctionnement interne, `capitalize` modifie le code ASCII de la première lettre. L'ASCII majuscule est différent de l'ASCII minuscule, et `capitalize` ajuste correctement la valeur.

## See Also / Voir Aussi :

- La documentation de Ruby sur les Strings : https://ruby-doc.org/core/String.html
- Un bon tuto sur les expressions régulières en Ruby : https://www.rubyguides.com/2015/06/ruby-regex/ 
- 'String#upcase' et 'String#downcase' pour d'autres transformations des chaînes : https://ruby-doc.org/core-3.0.0/String.html#method-i-upcase