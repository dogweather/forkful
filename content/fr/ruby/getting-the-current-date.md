---
title:                "Obtenir la date actuelle"
html_title:           "Ruby: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le obtenir?

Obtenir la date actuelle est une tâche courante pour les programmeurs. Cela leur permet de suivre le temps et de créer des fonctionnalités telles que des horodatages pour les données. Cette information est également utile pour les applications telles que les calendriers et les rappels.

# Comment le faire?

Dans Ruby, il existe une méthode simple pour obtenir la date actuelle. Voici un exemple de code:

```Ruby
current_date = Time.now
puts current_date
```

Cela affichera la date et l'heure actuelles dans le format suivant:

```Ruby
2021-11-16 11:45:23 +0100
```

Vous pouvez également personnaliser le format de la date en utilisant la méthode `strftime`, qui vous permet de spécifier le format dans lequel vous souhaitez afficher la date. Voici un exemple:

```Ruby
current_date = Time.now
puts current_date.strftime("%d/%m/%Y")
```

Cela affichera la date sous la forme jour/mois/année, comme ceci:

```Ruby
16/11/2021
```

# Plongée en profondeur

La méthode que nous avons utilisée pour obtenir la date actuelle, `Time.now`, a été introduite dans Ruby en 1999 dans la version 1.8.0. Avant cela, il existait des méthodes similaires comme `Date.today` et `DateTime.now`. Cependant, l'utilisation de `Time.now` est préférée car elle renvoie l'heure actuelle en secondes depuis l'époque Unix, ce qui peut être utile pour les calculs de temps.

Il est également important de noter que la méthode `Time.now` utilise l'horloge système de l'ordinateur, donc si l'heure système n'est pas correcte, la date renvoyée peut également être incorrecte.

Pour obtenir des informations plus précises sur la date et l'heure actuelles, comme le fuseau horaire ou la précision en nanosecondes, vous pouvez utiliser la classe `DateTime`. Voici un exemple:

```Ruby
current_datetime = DateTime.now
puts current_datetime
```

Cela affichera une date et heure précises, y compris le fuseau horaire, comme ceci:

```Ruby
2021-11-16T11:45:23+01:00
```

Si vous préférez utiliser une méthode en anglais plutôt qu'en français, vous pouvez également utiliser `DateTime.now` pour obtenir la même information.

# Voir aussi

Vous pouvez en apprendre davantage sur les méthodes de date et d'heure dans Ruby en consultant la documentation officielle https://ruby-doc.org/stdlib-2.7.5/libdoc/date/rdoc/Date.html et https://ruby-doc.org/stdlib-2.7.5/libdoc/time/rdoc/Time.html.