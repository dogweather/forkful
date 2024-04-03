---
date: 2024-01-20 17:46:39.440956-07:00
description: "Extraire des sous-cha\xEEnes en Ruby, c'est comme prendre un morceau\
  \ de votre baguette pr\xE9f\xE9r\xE9e : vous isolez une part de quelque chose de\
  \ plus grand. On le\u2026"
lastmod: '2024-03-13T22:44:58.406382-06:00'
model: gpt-4-1106-preview
summary: "Extraire des sous-cha\xEEnes en Ruby, c'est comme prendre un morceau de\
  \ votre baguette pr\xE9f\xE9r\xE9e : vous isolez une part de quelque chose de plus\
  \ grand."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## How to: (Comment faire :)
```Ruby
# Exemple 1: Extraire avec des indices
chaine = "Bonjour le monde!"
salutation = chaine[0,7] # Prend les caractères de l'index 0 à 6 (7 caractères au total)
puts salutation # Affiche "Bonjour"

# Exemple 2: Utiliser les indices négatifs
monde = chaine[-6..-2] # Les indices négatifs commencent à la fin de la chaîne
puts monde # Affiche "monde"

# Exemple 3: Extraire avec un Regexp
plat_favori = "J'adore les croissants au beurre!"
match = plat_favori[/croissants? au (\w+)/, 1] # Utilise une expression régulière pour matcher le mot après "au"
puts match # Affiche "beurre"
```

## Deep Dive (Plongeon en profondeur)
Historiquement, l'extraction de sous-chaînes est un concept hérité du traitement de texte en programmation. En Ruby, c'est intuitive et flexible, avec plusieurs méthodes possibles : les indices directement sur les chaînes, la méthode `slice`, ou des expressions régulières (`Regexp`).

Une alternative, c'est la méthode `split`, qui découpe une chaîne en un tableau selon un séparateur donné. En Ruby, `slice` et `split` sont souvent interchangeables, mais ils répondent à des besoins différents : `slice` pour un morceau précis et `split` pour découper en beaucoup de morceaux.

Concernant l'implémentation, Ruby gère les chaînes de caractères en encodage UTF-8 par défaut, ce qui signifie que l'extraction de sous-chaînes fonctionne bien avec des caractères accentués ou spéciaux en français, contrairement à de plus anciens langages où le codage était plus limité.

## See Also (Voir aussi)
- La documentation Ruby sur les chaînes de caractères : [String class](https://ruby-doc.org/core-2.7.0/String.html)
- Un tutoriel sur les expressions régulières en Ruby : [Ruby Regexp](https://www.rubyguides.com/2015/06/ruby-regex/)
