---
title:                "Suppression de caractères correspondant à un motif."
html_title:           "Ruby: Suppression de caractères correspondant à un motif."
simple_title:         "Suppression de caractères correspondant à un motif."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Supprimer des caractères correspondant à un modèle est une pratique courante chez les programmeurs pour nettoyer ou filtrer des données en fonction d'un certain modèle. Cela peut être utile pour formater des chaînes de texte ou pour supprimer des caractères indésirables tels que des espaces vides ou des caractères spéciaux.

## Comment faire:

Voici un exemple simple de code Ruby pour supprimer tous les chiffres d'une chaîne de caractères:

```Ruby
string = "Abc123def45ghi"
string.delete!("0-9") # supprime les chiffres de 0 à 9
puts string # output: Abcdefghi
```

Vous pouvez également utiliser une expression régulière pour supprimer des caractères correspondant à un modèle spécifique. Dans l'exemple ci-dessous, nous supprimons tous les caractères de ponctuation d'une chaîne de caractères:

```Ruby
string = "Hello, World!"
string.gsub!(/[[:punct:]]/, "") # supprime tous les caractères de ponctuation
puts string # output: HelloWorld
```

## Plongée en profondeur:

Cette pratique de suppression de caractères correspondant à un modèle tire son origine des expressions régulières (regex) qui sont des outils puissants pour la manipulation de chaînes de caractères. Ces expressions sont un langage de programmation en soi et peuvent être utilisées pour effectuer des recherches et des remplacements complexes dans du texte. Cependant, il existe également d'autres méthodes pour supprimer des caractères correspondant à un modèle, telles que la méthode `gsub` de Ruby ou des bibliothèques spécifiques comme `StringScanner`.

## Voir aussi:

Pour en savoir plus sur l'utilisation des expressions régulières, consultez la documentation officielle de Ruby: https://ruby-doc.org/core-3.0.1/Regexp.html

Vous pouvez également explorer les différentes méthodes de manipulation de chaînes de caractères de Ruby dans la documentation: https://ruby-doc.org/core-3.0.1/String.html