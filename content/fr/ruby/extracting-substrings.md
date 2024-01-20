---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Extraire des sous-chaînes, c'est scanner une chaîne de caractères pour isoler une partie spécifique. Les programmeurs le font pour nettoyer, analyser, ou manipuler des données.

## Comment faire:

Utilisation de la méthode de slice:
```Ruby
chaine = "Bonjour tout le monde!"
sous_chaine = chaine.slice(8,4) # retournera "tout"
```
Utilisation de la méthode de slice avec un index négatif:
```Ruby
chaine = "Bonjour tout le monde!"
sous_chaine = chaine.slice(-6,6) # retournera "monde!"
```
Utilisation d'une plage:
```Ruby
chaine = "Bonjour tout le monde!"
sous_chaine = chaine[8..11] # retournera "tout"
```

## Plongée en profondeur:

La capacité d'extraire des sous-chaînes a toujours été fondamentale en programmation, remontant aux premiers jours du langage C. Dans Ruby, la méthode `slice` présente depuis Ruby 1.8 fait un peu plus qu'extraire des sous-chaînes - elle peut également modifier la chaîne originale! Pour les fonctionnalités plus avancées ou spécifiques, on peut utiliser l'expression régulière.

Vous pouvez aussi utiliser la méthode `substring`. C'est très similaire à `slice`, mais elle ne peut pas modifier la chaîne originale.

En ce qui concerne les détails de mise en œuvre, la méthode `slice` est en réalité définie dans la classe `String`, qui fait partie de la bibliothèque standard de Ruby.

## Voir aussi:

- Documentation sur la méthode `slice` de Ruby : [https://ruby-doc.org/core-2.7.2/String.html#method-i-slice](https://ruby-doc.org/core-2.7.2/String.html#method-i-slice)
- Documentation officielle sur la classe `String`de Ruby : [https://ruby-doc.org/core-2.7.2/String.html](https://ruby-doc.org/core-2.7.2/String.html)