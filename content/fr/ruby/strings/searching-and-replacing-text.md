---
date: 2024-01-20 17:58:46.268553-07:00
description: "Chercher et remplacer du texte, c'est modifier des bouts de cha\xEE\
  nes de caract\xE8res automagiquement. Les devs le font pour corriger des erreurs,\
  \ mettre \xE0\u2026"
lastmod: '2024-03-13T22:44:58.402578-06:00'
model: gpt-4-1106-preview
summary: "Chercher et remplacer du texte, c'est modifier des bouts de cha\xEEnes de\
  \ caract\xE8res automagiquement. Les devs le font pour corriger des erreurs, mettre\
  \ \xE0\u2026"
title: Recherche et remplacement de texte
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi?)
Chercher et remplacer du texte, c'est modifier des bouts de chaînes de caractères automagiquement. Les devs le font pour corriger des erreurs, mettre à jour des données, ou transformer du texte rapidement sans se casser la tête.

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
