---
title:                "Ruby: Utiliser les expressions régulières"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Utiliser des expressions régulières peut considérablement simplifier votre code lorsque vous travaillez avec des chaînes de caractères. Elles permettent de rechercher et de manipuler des motifs de texte de manière efficace et rapide.

## Comment faire

Les expressions régulières en Ruby sont définies entre des barres obliques (slashes) et peuvent être utilisées avec des chaînes de caractères ou une variable contenant une chaîne de caractères.

```Ruby
# Recherche dans une chaîne de caractères
"Bonjour, je m'appelle Jean." =~ /Jean/  # retourne 19 (index de la première occurrence de "Jean")

# Utilisation de la méthode « scan » pour récupérer tous les mots commençant par « from »
"from date: 2021-01-01 to date: 2021-12-31" =~ /from[^ ]+/  # retourne ["from date:", "from date:"]
```

Il existe une variété de symboles spéciaux qui peuvent être utilisés pour créer des motifs plus complexes dans les expressions régulières. Par exemple, le symbole « ^ » peut être utilisé pour trouver des mots en début de ligne et le symbole « ? » pour rechercher des caractères optionnels. Vous pouvez trouver une liste complète de ces symboles et leur signification dans la documentation de Ruby.

## Plongée en profondeur

Les expressions régulières en Ruby peuvent être encore plus puissantes grâce à l'utilisation de blocs de code et de méthodes spéciales comme « sub » et « gsub ». Ces dernières permettent de remplacer une partie ou toutes les occurrences d'un motif dans une chaîne de caractères par un autre texte ou du code. Il est également possible de capturer des parties d'un motif pour les utiliser dans le remplacement.

Par exemple, vous pouvez utiliser cette fonctionnalité pour remplacer automatiquement des noms dans une phrase :

```Ruby
# Remplacer automatiquement une partie d'une phrase
"Il est parti en vacances avec son ami Pierre." =~ /(ami) ([A-Z][a-z]+)/
puts "Il est parti en vacances avec son #{$1} #{ $2.gsub(/[A-Z]/, '')}."  # retourne "Il est parti en vacances avec son ami pierre."
```

De plus, il existe des bibliothèques tierces, telles que « rubular » et « regexr », qui permettent de tester et de visualiser facilement vos expressions régulières en temps réel.

## Voir aussi

- [Documentation de Ruby sur les expressions régulières](https://ruby-doc.org/core-3.0.1/Regexp.html)
- [Liste de symboles pour les expressions régulières en Ruby](https://medium.com/@mekhifiras/tips-for-regex-in-ruby-9715be3c805c)
- [Bibliothèque rubular pour tester des expressions régulières en ligne](https://rubular.com/)
- [Bibliothèque regexr pour tester et visualiser des expressions régulières en ligne](https://regexr.com/)