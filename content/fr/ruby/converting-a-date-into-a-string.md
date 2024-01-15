---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Ruby: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des dates dans vos programmes Ruby, il peut être utile de les convertir en chaînes de caractères pour une meilleure lisibilité. Par exemple, au lieu d'avoir une date sous forme de nombres, vous pouvez l'afficher dans un format plus compréhensible pour l'utilisateur, comme "15 janvier 2020".

## Comment faire

Pour convertir une date en chaîne de caractères en Ruby, vous pouvez utiliser la méthode `strftime`. Voici un exemple de code pour convertir une date en format jour/mois/année dans une chaîne de caractères :

```ruby
date = Time.new(2020, 01, 15)
puts date.strftime("%d/%m/%Y")
```

La méthode `strftime` prend en paramètre une chaîne de formatage, qui va déterminer comment la date sera affichée. Dans cet exemple, `%d` correspond au jour, `%m` au mois et `%Y` à l'année. Vous pouvez consulter la documentation officielle de Ruby pour voir tous les formats disponibles.

Si vous souhaitez ajouter l'heure à votre chaîne de caractères, vous pouvez utiliser le formatage `%H` pour l'heure et `%M` pour les minutes :

```ruby
date = Time.new(2020, 01, 15, 14, 30)
puts date.strftime("%d/%m/%Y à %Hh%M")
```

Cela affichera la date sous la forme "15/01/2020 à 14h30".

## Plongée en profondeur

La méthode `strftime` peut sembler un peu compliquée au premier abord, mais elle est en réalité très puissante. Elle vous permet de formater une date de manière précise en utilisant différents symboles pour chaque élément (jour, mois, année, heure, etc.). En plus des formats de temps présentés ci-dessus, vous pouvez également utiliser des formats pour les jours de la semaine, les noms de mois, les fuseaux horaires, etc.

L'un des avantages de cette méthode est qu'elle est très portable. Elle fonctionne aussi bien avec les objets `Time` que `DateTime` ou `Date`. Elle est également disponible dans d'autres langages de programmation, tels que Python ou PHP.

Il est important de noter que le résultat de `strftime` sera toujours une chaîne de caractères. Si vous avez besoin de manipuler la date en tant qu'objet, vous pouvez utiliser la méthode `strptime` pour convertir une chaîne de caractères en date.

## Voir aussi
- [Documentation officielle sur `strftime`](https://ruby-doc.org/core-2.7.0/Time.html#method-i-strftime)
- [Liste complète des formats de `strftime`](https://docs.ruby-lang.org/en/2.6.0/Time/strftime.html)
- [Guide en français sur les dates en Ruby](https://www.rubyguides.com/2015/12/ruby-date-format/)