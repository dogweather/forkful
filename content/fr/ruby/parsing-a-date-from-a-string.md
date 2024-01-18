---
title:                "Analyser une date à partir d'une chaîne de caractères"
html_title:           "Ruby: Analyser une date à partir d'une chaîne de caractères"
simple_title:         "Analyser une date à partir d'une chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
En programmation, interpreter une date à partir d'une chaine de caractères (string) signifie extraire une date, formatée dans une forme cohérente, à partir d'une série de caractères. Les programmeurs font cela pour pouvoir manipuler les dates plus facilement et les utiliser dans leurs programmes.

## Comment faire:
Voici deux méthodes pour parser une date à partir d'une chaine de caractères en Ruby:

```Ruby
Date.parse("12/03/2020")
#=> #<Date: 2020-03-12 ((2458927j,0s,0n),+0s,2299161j)>

DateTime.parse("12/03/2020 18:22")
#=> #<DateTime: 2020-03-12T18:22:00+00:00 ((2458927j,66520s,0n),+0s,2299161j)>
```

La méthode `parse` convertit la chaine de caractères en objet `Date` ou `DateTime`, en fonction du format de la date fournie. Les formats acceptés incluent "mm/dd/yyyy", "dd/mm/yyyy", "yyyy-mm-dd", et bien plus encore.

## Plongée en profondeur:
Parser des dates à partir de chaines de caractères est devenu un besoin essentiel avec la montée en popularité des applications web et mobile. Les alternatives à Ruby incluent le langage de programmation Python, qui dispose d'une bibliothèque dédiée à la manipulation de dates appelée "datetime". En interne, le parsing des dates en Ruby repose sur les méthodes `parse` et `strptime` de la classe `Time`. La première utilise une analyse de forme libre pour détecter le format de la date et la convertir, tandis que la seconde utilise une expression régulière définie pour détecter le format.

## Voir aussi:
- [La documentation officielle de Ruby pour la classe Date](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Un article sur le parsing de dates en utilisant la méthode `strptime`](https://medium.com/@grecio/date-parsing-in-ruby-f9d6b77ec569)
- [La documentation officielle de Python pour la bibliothèque "datetime"](https://docs.python.org/fr/3/library/datetime.html)