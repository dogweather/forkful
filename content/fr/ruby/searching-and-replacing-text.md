---
title:                "Ruby: Recherche et remplacement de texte"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de remplacer du texte dans un programme pour corriger des erreurs ou pour modifier du contenu. Utiliser la fonction de recherche et de remplacement en Ruby peut vous faire gagner du temps et vous aider à éviter des erreurs manuelles.

## Comment faire

Pour utiliser la fonction de recherche et de remplacement en Ruby, il vous suffit d'utiliser la méthode `gsub` (global substitution) sur une chaîne de caractères. Voici un exemple de code :

```Ruby
texte = "Bonjour le monde!"
texte.gsub!("monde", "Ruby")

puts texte #=> Bonjour le Ruby!
```
Dans cet exemple, nous avons défini une variable `texte` avec une phrase contenant le mot "monde". En utilisant la méthode `gsub`, nous avons remplacé le mot "monde" par "Ruby". En exécutant ce code, nous obtenons la phrase modifiée "Bonjour le Ruby!".

Vous pouvez également utiliser la méthode `gsub` avec des expressions régulières pour effectuer des remplacements plus complexes. Voici un autre exemple de code :

```Ruby
texte = "J'aime manger du fromage."
texte.gsub!(/manger (.*) fromage/, 'manger du bon fromage')

puts texte #=> J'aime manger du bon fromage.
```
Dans cet exemple, nous avons utilisé une expression régulière pour cibler une partie spécifique de la phrase et la remplacer par une nouvelle phrase.

## Plongée en profondeur

La méthode `gsub` accepte également un bloc en tant que paramètre, ce qui vous permet de personnaliser encore plus votre recherche et votre remplacement de texte. Vous pouvez y accéder en utilisant `gsub!` avec un bloc comme ceci :

```Ruby
texte = "Il fait beau dehors."
texte.gsub!("beau") { |match| match.upcase }

puts texte #=> Il fait BEAU dehors.
```

Dans cet exemple, nous avons utilisé le bloc pour mettre en majuscule le mot "beau" dans notre phrase. Vous pouvez également utiliser le bloc pour effectuer des modifications plus complexes, telles que la transformation d'un mot en une autre forme grammaticale.

## Voir aussi

- [Guide sur les méthodes de manipulation de chaînes en Ruby](https://www.rubyguides.com/ruby-tutorial/string-methods/)
- [Documentation officielle de Ruby sur la méthode `gsub`](https://ruby-doc.org/core-2.7.3/String.html#method-i-gsub)
- [Tutoriel sur les expressions régulières en Ruby](https://www.regular-expressions.info/ruby.html)