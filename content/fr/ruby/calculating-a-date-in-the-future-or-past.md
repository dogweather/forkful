---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Ruby: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Êtes-vous fatigué de calculer manuellement une date dans le futur ou dans le passé? Ou peut-être que vous cherchez simplement à automatiser cette tâche fastidieuse? Dans tous les cas, l'utilisation de Ruby peut grandement vous faciliter la vie.

## Comment faire

Tout d'abord, vous aurez besoin d'importer la bibliothèque standard Date de Ruby en ajoutant ```require 'date'``` au début de votre code.

### Calculer une date dans le futur

Pour calculer une date dans le futur, vous pouvez utiliser la méthode ```Date.today + n```, où ```n``` est le nombre de jours dans le futur que vous souhaitez calculer. Par exemple, si vous voulez obtenir la date d'un mois à partir d'aujourd'hui, vous pouvez utiliser ```Date.today + 30```.

### Calculer une date dans le passé

Pour calculer une date dans le passé, vous pouvez utiliser la méthode ```Date.today - n```, où ```n``` est le nombre de jours dans le passé que vous souhaitez calculer. Par exemple, si vous voulez obtenir la date il y a un mois à partir d'aujourd'hui, vous pouvez utiliser ```Date.today - 30```.

### Exemples de code et résultats

Voici un exemple de code pour calculer une date dans le futur:

```
require 'date'
future_date = Date.today + 15
puts "Dans 15 jours, nous serons le #{future_date}"
```
Résultat:
```
Dans 15 jours, nous serons le 2021-10-07
```

Et voici un exemple de code pour calculer une date dans le passé:

```
require 'date'
past_date = Date.today - 30
puts "Il y a un mois, nous étions le #{past_date}"
```
Résultat:
```
Il y a un mois, nous étions le 2021-08-11
```

## Plongée en profondeur

Ruby offre également d'autres méthodes pour calculer des dates dans le futur ou dans le passé, telles que ```Date.new```, ```Date.parse```, et ```Date.strptime```. Vous pouvez également spécifier des formats de date spécifiques en utilisant la méthode ```strftime```. Consultez la documentation officielle de Ruby pour en savoir plus sur ces méthodes.

## Voir aussi

- [Documentation officielle de Ruby](https://ruby-doc.org/)
- [Guide de référence rapide de Ruby](https://www.ruby-lang.org/fr/documentation/quickstart/)