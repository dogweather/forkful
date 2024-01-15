---
title:                "Recherche et remplacement de texte"
html_title:           "Ruby: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## pourquoi 

Si vous travaillez avec du texte dans vos programmes Ruby, vous voudrez peut-être le modifier à un moment donné. Cela peut être pour des raisons telles que la correction d'erreurs ou la mise à jour de données obsolètes. Dans ces cas, la recherche et le remplacement de texte peuvent être des tâches très utiles et efficaces.  

## Comment 

La recherche et le remplacement de texte peuvent être réalisés en utilisant la méthode `gsub` de la classe `String`. Voici un exemple de code: 

```Ruby 
texte = "Bonjour tout le monde" 
texte.gsub!("bonjour", "salut") 
puts texte 
```

Cela devrait produire une sortie de "Salut tout le monde". Dans cet exemple, nous avons remplacé le mot "bonjour" par "salut" dans la variable `texte` à l'aide de la méthode `gsub`. 

## Plongée en profondeur 

Il est également possible d'utiliser des expressions régulières lors de la recherche et du remplacement de texte. Par exemple, si vous voulez remplacer toutes les voyelles dans une chaîne par des tirets, vous pourriez utiliser cette expression régulière: `/[aeiou]/`. Voici un exemple de code avec cette expression régulière: 

```Ruby 
texte = "Bonjour tout le monde" 
texte.gsub!(/[aeiou]/, "-") 
puts texte 
```

Cela produirait une sortie de "B-nj--r t--t l- m-nd-". Les expressions régulières peuvent être très puissantes pour trouver et remplacer des motifs spécifiques dans une chaîne de texte. 

## Voir aussi 

- La documentation de la méthode `gsub`: https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub 
- Un tutoriel sur les expressions régulières en Ruby: https://www.rubyguides.com/2015/06/ruby-regular-expressions/