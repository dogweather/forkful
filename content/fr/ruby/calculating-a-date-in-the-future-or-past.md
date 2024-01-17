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

## Quoi et Pourquoi?

Calculer une date dans le futur ou dans le passé est une tâche couramment effectuée par les programmeurs afin de réaliser des opérations temporelles dans leurs programmes. Par exemple, cela pourrait permettre de déterminer la date d'expiration d'un abonnement ou la date de livraison d'une commande en ligne.

## Comment faire:

Voici un exemple de code en Ruby pour calculer une date dans le futur en utilisant la méthode `advance` :

```Ruby
future_date = Date.today.advance(days: 10)
puts future_date #=> 2020-10-27
```

Et voici un exemple pour calculer une date dans le passé en utilisant la méthode `days_ago` :

```Ruby
past_date = Date.today.days_ago(7)
puts past_date #=> 2020-10-13
```

## Plongée en profondeur:

Historiquement, le calcul de dates dans le futur ou dans le passé a été un défi pour les programmeurs, car cela impliquait une prise en compte des années bissextiles et des différents calendriers utilisés à travers le monde. Cependant, avec l'avènement de bibliothèques de gestion de dates telles que Ruby's `Date`, cette tâche est désormais simplifiée et gérée de manière transparente.

Dans le cas où vous préférez utiliser une bibliothèque différente pour gérer les dates, il existe plusieurs alternatives telles que `Chrono` ou `Timecop` qui peuvent être utilisées avec Ruby pour gérer les opérations temporelles.

## À voir également:

Pour en savoir plus sur la gestion des dates en Ruby, vous pouvez consulter la documentation officielle de Ruby sur les objets `Date` et `DateTime` ainsi que des tutoriels et des articles en ligne pour découvrir plus de détails sur la gestion des dates dans le langage de programmation Ruby.