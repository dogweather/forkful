---
title:    "Ruby: Calculer une date dans le futur ou le passé"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Les dates sont un élément fondamental de la programmation et il est souvent nécessaire de calculer une date dans le futur ou le passé dans de nombreux projets. Cela peut être utile pour des tâches telles que la planification d'événements ou la gestion d'abonnements.

## Comment faire

Pour calculer une date dans le futur ou le passé en utilisant Ruby, nous pouvons utiliser la méthode `strftime` de la classe `Date`. Cette méthode nous permet de formater une date en utilisant des spécificateurs de format. Voici un exemple de code pour calculer une date dans le futur en utilisant un intervalle de jours :

```Ruby
future_date = Date.today + 10 # 10 jours dans le futur
puts future_date.strftime("%d/%m/%Y") #formatage de la date en jour/mois/année
```

Cela produira la sortie suivante :

```
25/09/2021
```

## Plongeons plus profondément

La méthode `strftime` prend en compte de nombreux spécificateurs de format pour manipuler les dates. Par exemple, `%d` représente le jour du mois, `%B` représente le nom du mois, `%Y` représente l'année sur quatre chiffres, etc. Vous pouvez trouver une liste complète des spécificateurs de format dans la [documentation de Ruby](https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime).

Il est également important de noter que la méthode `strftime` peut être utilisée avec d'autres classes de date comme `Time` et `DateTime`.

## Voir aussi

- [Documentation de Ruby sur strftime](https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime)
- [Gestion des dates et heures avec Ruby](https://www.rubyguides.com/2018/01/ruby-dates/)