---
date: 2024-01-20 17:35:33.800521-07:00
description: "How to (Comment Faire) Concat\xE9ner avec l'op\xE9rateur `+` ."
lastmod: '2024-04-05T21:53:59.814647-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## How to (Comment Faire)
Concaténer avec l'opérateur `+` :

```Ruby
salutation = "Bonjour" + ", " + "monde!"
puts salutation
# Affiche : Bonjour, monde!
```

Avec interpolation :

```Ruby
nom = "Alice"
message = "Salut #{nom}!"
puts message
# Affiche : Salut Alice!
```

Concaténer avec `<<` qui modifie la chaîne en place :

```Ruby
base = "Ruby"
base << " est" << " incroyable!"
puts base
# Affiche : Ruby est incroyable!
```

## Deep Dive (Plongée en Profondeur)
Historiquement, la concaténation était déjà présente dans des langages anciens comme C ou Perl, mais Ruby la simplifie avec une syntaxe claire. Alternatives : `.concat` ou `.join` pour assembler des tableaux de chaînes. L'interpolation est plus propre que la concaténation avec `+` car elle évite de convertir des types non-string et gère mieux la mémoire.

```Ruby
nombres = ["un", "deux", "trois"]
texte = nombres.join(", ")
# "un, deux, trois"
```

En ce qui concerne l'implémentation, Ruby gère les chaînes de caractères comme des séquences de bytes. Utiliser `+` crée un nouvel objet string à chaque fois, tandis que `<<` modifie l'original, ce qui est plus efficace en termes de performance.

## See Also (Voir Aussi)
- [Ruby Doc sur les Strings](https://ruby-doc.org/core-3.0.0/String.html)
