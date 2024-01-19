---
title:                "Convertir une chaîne en minuscules"
html_title:           "PHP: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Convertir une chaîne en minuscules signifie modifier tous les caractères majuscules d'une chaîne pour devenir minuscules. Les programmeurs le font pour uniformiser les données, par exemple, pour faciliter les comparaisons sans tenir compte des différences de casse.

## Comment faire:
En Ruby, vous pouvez utiliser la méthode `downcase` pour convertir une chaîne en minuscules. Voici un exemple:

```Ruby
ma_chaine = "BONJOUR LE MONDE"
puts ma_chaine.downcase
```

Et voilà le résultat que vous obtenez:

```
bonjour le monde
```

## Plongeon Profond
Historiquement, `downcase` est une méthode standard dans les anciennes versions de Ruby. Les alternatives incluent la méthode `swapcase` qui inverse la casse des caractères dans une chaîne. Les détails de mise en œuvre de la méthode `downcase` sont plutôt simples: elle parcourt chaque caractère de la chaîne and, si c'est une lettre majuscule, la convertit en minuscule.

## Voir Aussi 
Pour en savoir plus sur la méthode `downcase` et autres méthodes de chaîne en Ruby, consultez les ressources suivantes:

- Documentation Ruby pour la classe String: [https://ruby-doc.org/core-2.7.1/String.html](https://ruby-doc.org/core-2.7.1/String.html)
- Billet de blog 'Ruby Explained: String case Methods': [http://rubylearning.com/blog/2011/07/28/ruby-explained-string-case-methods/](http://rubylearning.com/blog/2011/07/28/ruby-explained-string-case-methods/)