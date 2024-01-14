---
title:    "Ruby: Transformation d'une date en chaîne de caractères."
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaîne de caractères est une tâche commune lors du développement d'applications en Ruby. Elle est souvent nécessaire pour afficher la date dans un format spécifique ou pour interagir avec des données stockées sous forme de chaîne de caractères. Dans cet article, nous allons explorer comment effectuer cette conversion en utilisant Ruby.

## Comment faire

Pour convertir une date en chaîne de caractères, nous utiliserons la méthode `strftime()` qui permet de formater une date selon un modèle spécifié. Voici un exemple de code qui utilise cette méthode pour convertir la date du jour en une chaîne de caractères au format DD/MM/YYYY :

```Ruby
date = Date.today
date_str = date.strftime("%d/%m/%Y")
puts date_str
```

La sortie de ce code sera `21/10/2021`.

Nous pouvons également spécifier un autre modèle pour formater la date, par exemple `%m-%d-%Y` pour obtenir une date au format MM-DD-YYYY. Vous pouvez consulter la liste complète des directives de formatage disponibles dans la documentation officielle de Ruby.

## Deep Dive

La méthode `strftime()` est une méthode de la classe `Time` en Ruby, mais elle peut également être utilisée sur les objets de la classe `Date` car cette dernière hérite des méthodes de la classe `Time`. Il est important de noter que les objets `DateTime` ne peuvent pas utiliser la méthode `strftime()` car ils ne dérivent pas de la classe `Time`.

De plus, la méthode `strftime()` accepte un argument facultatif pour spécifier une langue différente pour le formatage de la date. Par exemple, pour obtenir la date en français, nous pouvons utiliser `%A, %d %B %Y` comme modèle et `:fr` comme argument.

## Voir aussi

- [Documentation officielle de Ruby sur la méthode strftime()](https://ruby-doc.org/core-2.6.3/Time.html#method-i-strftime)
- [Article de blog sur la manipulation des dates en Ruby](https://code.tutsplus.com/fr/tutorials/ruby-for-newbies-working-with-dates-and-time--net-31771)
- [Guide de référence complète des directives de formatage de dates en Ruby](https://www.guru99.com/ruby-date-time-class.html#11)