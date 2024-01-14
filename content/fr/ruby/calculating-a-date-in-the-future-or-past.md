---
title:                "Ruby: Calculer une date dans le futur ou le passé"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi voudriez-vous calculer une date dans le futur ou le passé en programmation ? Eh bien, cela pourrait être utile pour afficher des rappels de rendez-vous ou des dates d'expiration, ou pour planifier des tâches de manière dynamique en fonction de la date actuelle.

## Comment faire

La manipulation des dates peut sembler intimidante, mais en utilisant la puissance de Ruby, c'est assez simple. Tout d'abord, nous devons comprendre comment les dates sont représentées en Ruby. Les dates sont stockées sous forme d'objets de la classe `Date`. Voici un exemple de code pour créer un objet `Date` pour le 1er janvier 2022 :

```Ruby
date = Date.new(2022, 1, 1)
puts date
```

La sortie de ce code serait `2022-01-01`, ce qui est la date dans le format ISO 8601. Maintenant, pour calculer une date dans le futur ou le passé, nous utilisons la méthode `+` et `-` sur un objet `Date` pour ajouter ou soustraire un certain nombre de jours. Par exemple, si nous voulons obtenir la date 30 jours dans le futur à partir de notre objet `date`, nous pouvons utiliser le code suivant :

```Ruby
future_date = date + 30
puts future_date
```

La sortie serait `2022-01-31`. De même, si nous voulons calculer une date dans le passé, nous utilisons simplement la méthode `-`. Par exemple, pour obtenir la date 15 jours avant notre objet `date`, nous pouvons utiliser le code suivant :

```Ruby
past_date = date - 15
puts past_date
```

La sortie serait `2021-12-17`. Mais que se passe-t-il si nous voulons calculer une date en fonction de la date actuelle ? Dans ce cas, nous pouvons utiliser la méthode `Date.today`, qui renvoie la date actuelle, et ensuite utiliser les méthodes `+` et `-` pour calculer la date souhaitée.

## Plongée en profondeur

En utilisant les méthodes de calcul de date que nous avons vues ci-dessus, vous pouvez effectuer de nombreuses opérations complexes en combinant différentes manipulations de date. Par exemple, vous pouvez facilement vérifier si une date se situe dans un certain intervalle de temps en utilisant les opérateurs `>=` et `<=`. Vous pouvez également utiliser des méthodes pratiques telles que `Date.tomorrow` ou `Date.yesterday` pour obtenir la date du lendemain ou du jour précédent respectivement.

Il est également important de comprendre comment les dates sont calculées en fonction des années bissextiles et des mois avec un nombre de jours variable. Ruby gère cela de manière transparente, il vous suffit donc de vous concentrer sur votre code et non sur les spécificités du calendrier.

## Voir aussi

- [La documentation officielle de Ruby sur les objets Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Un guide en français sur la manipulation des dates en Ruby](https://rubyguides.com/travail-dates-temps-ruby/)