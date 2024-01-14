---
title:                "Ruby: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Êtes-vous curieux de savoir comment obtenir la date actuelle en utilisant Ruby? Ou peut-être avez-vous un projet qui nécessite de connaître la date et l'heure exactes? Dans cet article, nous allons examiner comment obtenir la date actuelle en utilisant Ruby et pourquoi cela peut être utile pour vos projets de programmation.

## Comment faire

Obtenir la date actuelle est assez simple en utilisant Ruby. Nous pouvons utiliser la méthode `Time.now` pour obtenir un objet de type `Time` qui représente la date et l'heure actuelles. Voyons un exemple concret en utilisant la console Ruby:

```Ruby
Time.now
```
Résultat:
`2021-01-24 15:30:00 +0200`

Comme on peut le voir, nous obtenons la date au format "AAAA-MM-JJ HH:MM:SS +HHMM". Cependant, il est souvent plus pratique de formater la date selon nos besoins. Nous pouvons le faire en utilisant la méthode `strftime` et en spécifiant le format que nous voulons. Par exemple:

```Ruby
Time.now.strftime("%d/%m/%Y")
```
Résultat:
`24/01/2021`

Nous pouvons également obtenir des informations spécifiques telles que l'année, le mois et le jour en utilisant les méthodes `year`, `month` et `day`, respectivement. Par exemple:

```Ruby
Time.now.year
```
Résultat:
`2021`

Il est également possible de modifier la date en utilisant les méthodes `change` et `advance`. Par exemple, si nous voulons obtenir la date dans 10 jours:

```Ruby
Time.now.advance(days: 10)
```
Résultat:
`2021-02-03 15:30:00 +0200`

Ces sont seulement quelques exemples de la façon d'utiliser la méthode `Time.now` pour obtenir la date actuelle en utilisant Ruby. Pour en savoir plus sur les possibilités de cette méthode, consultez la documentation officielle de Ruby.

## Plongée en profondeur

Maintenant que nous avons vu comment obtenir la date actuelle, il est important de comprendre comment fonctionne la méthode `Time.now` en profondeur. En fait, cette méthode retourne un objet de type `Time` qui représente un point dans le temps spécifique. En d'autres termes, il s'agit d'un instant précis dans le temps depuis une date de référence appelée Epoch (1er janvier 1970).

De plus, la classe `Time` offre des fonctionnalités supplémentaires telles que les méthodes de comparaison et d'arithmétique, ce qui la rend très utile pour manipuler des dates et des heures dans nos programmes.

## Voir aussi

- Documentation officielle de Ruby sur la méthode `Time.now`: https://ruby-doc.org/core-3.0.0/Time.html#method-c-now
- Tutoriel sur la gestion des dates et heures en Ruby: https://www.rubyguides.com/2015/10/ruby-time/
- Une introduction à la programmation en Ruby: https://www.simplilearn.com/learn-ruby-on-rails-basics-skillup