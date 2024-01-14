---
title:                "Ruby: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi 

De nombreuses tâches de programmation nécessitent la manipulation de chaînes de texte, et parfois il est nécessaire de remplacer du texte spécifique dans ces chaînes. Heureusement, en utilisant quelques astuces de Ruby, vous pouvez effectuer ces recherches et remplacements de manière efficace et précise.

## Comment faire 

Pour remplacer du texte dans une chaîne en utilisant Ruby, vous pouvez utiliser la méthode `sub`, qui prend en argument une expression régulière pour la chaîne à rechercher et une autre chaîne pour la remplacer. Par exemple:

```Ruby 
phrase = "Je suis un programmeur Ruby."
nouvelle_phrase = phrase.sub(/Ruby/, "Python")
puts nouvelle_phrase
```

Ceci produira la sortie: "Je suis un programmeur Python." La méthode `sub` remplacera uniquement la première occurrence du texte recherché dans la chaîne. Si vous souhaitez remplacer toutes les occurrences, vous pouvez utiliser la méthode `gsub`.

```Ruby
phrase = "Ruby est un langage de programmation populaire."
nouvelle_phrase = phrase.gsub(/Ruby/, "Python")
puts nouvelle_phrase 
```

Cela produira la sortie: "Python est un langage de programmation populaire." Vous pouvez également utiliser des expressions régulières plus complexes pour effectuer des recherches et remplacements en utilisant la méthode `sub`. Par exemple, en utilisant des groupes dans votre expression régulière, vous pouvez capturer des parties de la chaîne d'origine et les utiliser dans la chaîne de remplacement.

## Plongée en profondeur 

Il existe plusieurs autres méthodes et options en Ruby pour effectuer des recherches et remplacements dans du texte, telles que `scan` et `replace`. En utilisant ces méthodes, vous pouvez créer des scripts puissants pour manipuler des chaînes de texte à votre avantage. De plus, comprendre les expressions régulières en profondeur vous permettra de créer des recherches et remplacements encore plus complexes et précis.

## Voir aussi 

- [Documentation officielle de la méthode `sub`](https://ruby-doc.org/core-2.5.1/String.html#method-i-sub)
- [Documentation officielle de la méthode `gsub`](https://ruby-doc.org/core-2.5.1/String.html#method-i-gsub)
- [Documentation officielle de la méthode `scan`](https://ruby-doc.org/core-2.5.1/String.html#method-i-scan)
- [Documentation officielle de la méthode `replace`](https://ruby-doc.org/core-2.5.1/String.html#method-i-replace)