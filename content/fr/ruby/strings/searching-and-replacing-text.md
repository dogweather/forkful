---
date: 2024-01-20 17:58:46.268553-07:00
description: "How to: (Comment faire:) Chercher et remplacer est une op\xE9ration\
  \ vieille comme le monde informatique. En Ruby, `#gsub!` et `#sub!` sont les m\xE9\
  thodes les\u2026"
lastmod: '2024-04-05T22:51:12.261182-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire:) Chercher et remplacer est une op\xE9ration vieille comme\
  \ le monde informatique."
title: Recherche et remplacement de texte
weight: 10
---

## How to: (Comment faire:)
```Ruby
# Exemple simple pour remplacer une sous-chaîne
phrase = "Les frites sont cuites!"
phrase.gsub!("cuites", "exquises")
puts phrase
# => Les frites sont exquises!

# Utilisation d'une expression régulière pour remplacer tout chiffre par 'X'
numero_securite_sociale = "2-41-15-64-321"
numero_securite_sociale.gsub!(/\d/, 'X')
puts numero_securite_sociale
# => X-XX-XX-XX-XXX

# Remplacement conditionnel avec un bloc
texte = "Ruby est jovial. Ruby est élégant. Ruby est génial."
texte.gsub!(/Ruby/) { |match| match == "Ruby" ? "Le Ruby" : match }
puts texte
# => Le Ruby est jovial. Le Ruby est élégant. Le Ruby est génial.
```

## Deep Dive (Plongée en profondeur)
Chercher et remplacer est une opération vieille comme le monde informatique. En Ruby, `#gsub!` et `#sub!` sont les méthodes les plus fréquentes pour ça. Elles se distinguent par le nombre de remplacements effectués: `#sub!` remplace la première occurrence, `#gsub!` toutes. Les expressions régulières (regex) sont un super pouvoir Ruby pour chercher avec précision ; un outil que tous les programmeurs devraient maîtriser.

En matière d'alternatives, des outils comme `sed` en ligne de commande ou des éditeurs de texte comme `vim` ou `VSCode` proposent des fonctionnalités similaires. Mais réaliser ces opérations dans Ruby même est souvent plus rapide et plus scriptable - surtout pour les manipulations de données et l'automatisation de tâches répétitives.

## See Also (Voir également)
- Documentation Ruby officielle sur les méthodes `gsub` et `gsub!`: [Ruby Docs](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub)
- Une explication détaillée des expressions régulières en Ruby: [Ruby Regexp](https://ruby-doc.org/core-3.1.2/Regexp.html)
- Tutoriel interactif sur les Regex: [Rubular](http://rubular.com/)
