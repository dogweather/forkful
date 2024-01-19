---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi ?

La conversion d'une date en chaîne de caractères est une opération qui transforme un objet Date en texte formaté. Les programmeurs le font pour faciliter l'affichage des dates et le traitement sous forme de texte.

## Comment faire :

Vous pouvez convertir une date en chaîne de caractères en Ruby en utilisant la méthode `strftime`. Voici un exemple simple :

```Ruby
# Créer une nouvelle date
date = Date.new(2021, 10, 31)

# Converter la date en une chaîne de caractères
date_string = date.strftime("%d/%m/%Y")

puts date_string
# Sortie : 31/10/2021
```

## Détails

Ruby a adopté la méthode strftime du langage C plus ancien, elle est donc présente depuis les premières versions. Il existe d'autres moyens, comme la méthode `to_s`, mais `strftime` offre une flexibilité incomparable pour formater les dates.

La conversion est une action en mémoire et n'a pas d'impact direct sur l'objet Date original. C'est une opération à sens unique, la chaîne obtenue ne peut pas être retransformée en date sans un supplément de logique.

## Voir aussi

Pour davantage de détails sur la méthode strftime et ses options de formatage, consultez la documentation officielle Ruby : https://ruby-doc.org/stdlib-2.6.1/libdoc/date/rdoc/Date.html#method-i-strftime

Et pour une introduction générale aux dates et heures en Ruby, jetez un oeil à ce guide : https://www.rubyguides.com/ruby-tutorial/ruby-date-format/