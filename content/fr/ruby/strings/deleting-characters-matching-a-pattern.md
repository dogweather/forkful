---
date: 2024-01-20 17:43:14.789988-07:00
description: "Supprimer des caract\xE8res qui correspondent \xE0 un motif, c'est filtrer\
  \ les s\xE9quences ind\xE9sirables de votre texte. Les programmeurs le font pour\
  \ nettoyer les\u2026"
lastmod: '2024-03-11T00:14:32.273354-06:00'
model: gpt-4-1106-preview
summary: "Supprimer des caract\xE8res qui correspondent \xE0 un motif, c'est filtrer\
  \ les s\xE9quences ind\xE9sirables de votre texte. Les programmeurs le font pour\
  \ nettoyer les\u2026"
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Supprimer des caractères qui correspondent à un motif, c'est filtrer les séquences indésirables de votre texte. Les programmeurs le font pour nettoyer les données, valider des entrées ou manipuler des chaînes pour l'analyse.

## Comment faire :

```Ruby
# Supprimer tous les chiffres d'une chaîne
texte = "Voici le numéro 12345."
texte_sans_chiffres = texte.gsub(/[0-9]/, '')
puts texte_sans_chiffres
# => Voici le numéro .

# Supprimer des caractères spécifiques
email = "example[at]domain[dot]com"
email_normalise = email.gsub(/\[at\]/, '@').gsub(/\[dot\]/, '.')
puts email_normalise
# => example@domain.com

# Utiliser 'delete' pour enlever plusieurs caractères
slogan = "Ruby: Pur & Simple!"
slogan_sans_signes = slogan.delete("&!")
puts slogan_sans_signes
# => Ruby: Pur Simple
```

## Plongée Profonde

Historiquement, `gsub` et `delete` sont des méthodes qui viennent des origines du langage Ruby. Elles sont conçues pour manipuler des chaînes de caractères de manière intuitive. L’alternative, `gsub!` et `delete!`, modifient la chaîne en place, ce qui peut être utile pour économiser la mémoire. 

Utiliser des expressions régulières avec `gsub` permet une grande flexibilité pour spécifier des motifs complexes. Mais attention, les expressions régulières peuvent ralentir votre code si elles sont mal utilisées. 

Pour implémenter ces fonctions efficacement, Ruby utilise un moteur d'expressions régulières robuste. Il est conçu pour traiter rapidement du texte, tout en offrant un large éventail de motifs et opérateurs de correspondance.

## Voir Aussi

- La documentation de Ruby sur les expressions régulières : [Regexp](https://ruby-doc.org/core-3.1.2/Regexp.html)
- Documentation sur `String#gsub`: [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub)
- Documentation sur `String#delete`: [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete)
- Ruby Style Guide pour les bonnes pratiques de codage : [Ruby Style Guide](https://rubystyle.guide/)
- Un tutoriel sur les expressions régulières en Ruby : [Ruby Regexp Tutorial](https://www.rubyguides.com/2015/06/ruby-regex/)
