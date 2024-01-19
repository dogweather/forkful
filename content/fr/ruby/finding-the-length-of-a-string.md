---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La recherche de la longueur d'une chaîne signifie déterminer le nombre de caractères qu'elle contient. Les programmeurs le font souvent lorsqu'ils doivent manipuler ou valider des données textuelles.

## Comment faire:

Voici comment vous pouvez le faire en Ruby :

```Ruby
chaîne = "Bonjour tout le monde"
longueur = chaîne.length
puts longueur
```

La sortie de ce code sera :

```Ruby
21
```

## Plongée en profondeur

Partie intégrante des fonctions de base de nombreux langages de programmation, la recherche de la longueur d'une chaîne a été introduite en Ruby dès sa première version en 1995. Pour des raisons d'efficacité et de performances, cette méthode est généralement implémentée en comptant le nombre de caractères binaires dans la chaîne plutôt que les caractères individuels.

En plus de `length`, Ruby offre quelques autres méthodes pour faire le même travail comme `size` et `bytesize`. Tandis que `length` et `size` renvoient le nombre de caractères de la chaîne, `bytesize` renvoie le nombre de bytes. Cela peut être différent, surtout si vous travaillez avec des chaînes Unicode.

## Voir aussi

Pour apprendre encore plus sur les chaînes en Ruby, consultez les liens ci-dessous :

- La documentation officielle de Ruby sur les chaînes :
    http://ruby-doc.org/core-2.7.0/String.html

- Une introduction aux chaînes en Ruby par SitePoint :
    https://www.sitepoint.com/guide-ruby-strings/
  
- Un article sur le travail avec les chaînes en Ruby par RubyGuides :
    https://www.rubyguides.com/2015/01/ruby-string-methods/  

Rappelez-vous, la programmation est un voyage. Continuez à apprendre, continuez à programmer!