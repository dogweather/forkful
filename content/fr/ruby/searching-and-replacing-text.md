---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# À la découverte de la recherche et du remplacement de texte en Ruby

## Quoi & Pourquoi?

La recherche et le remplacement de texte sont deux opérations essentielles en programmation. Elles vous permettent de modifier un texte en trouvant des segments spécifiques puis en les remplaçant par d'autres. C'est un outil précieux pour la manipulation et l'aménagement de données.

## Comment faire:

Voici un exemple de recherche et de remplacement en Ruby:

```Ruby
text = "Je déclare mon amour pour Ruby."
updated_text = text.sub("amour pour", "haine envers")
puts updated_text
```
Le résultat d'exécution de ce code sera:

```Ruby
"Je déclare ma haine envers Ruby."
```

Dans cet exemple, `sub` est une méthode Ruby qui trouve la première occurrence de la chaîne "amour pour" et la remplace par "haine envers".

## Exploration en profondeur:

Historiquement, le remplacement de texte a toujours été une partie vitale de la programmation. En effet, la capacité à manipuler des chaînes de caractères est un aspect fondamental de nombreux processus informatiques, allant de la modification des entrées utilisateur à la transformation de larges bases de données.

Ruby offre plusieurs méthodes pour manipuler des textes. En plus de `sub`, Ruby fournit également une méthode `gsub` (substituer globalement) qui remplacera toutes les occurrences d'une chaîne, pas seulement la première. Voici un exemple:

```Ruby
text = "Ruby est incroyable. J'adore Ruby."
updated_text = text.gsub("Ruby", "Python")
puts updated_text
```

Le résultat sera:

```Ruby
"Python est incroyable. J'adore Python."
```

## Voir aussi:

- Documentation Ruby sur les chaînes : [Ruby Doc: String](https://ruby-doc.org/core-3.1.0/String.html)
- Tutoriel sur la manipulation de chaînes en Ruby : [RubyLearning: String Manipulation](http://rubylearning.com/satishtalim/ruby_strings.html)