---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi ?

Comparer deux dates consiste à déterminer quelle date est la plus récente entre deux. Les programmeurs le font pour gérer les horodatages, les calendriers, les alertes, et toutes sortes de fonctionnalités basées sur le temps.

## Comment faire :

Voyons comment vous pouvez comparer deux dates en PHP. Ici, nous allons utiliser la fonction `DateTime::diff()`. Voici un exemple :
```PHP
$date1 = new DateTime('2022-02-15');
$date2 = new DateTime('2023-01-01');

$intervalle = $date1->diff($date2);

echo $intervalle->format('%R%a jours');
```
Cette programmation vous affiche la différence entre les deux dates, `$date1` et `$date2`, en jours. Si `$date2` est plus récente, elle affiche un nombre positif, sinon un nombre négatif.

## Approfondissement :

Historiquement, on pouvait comparer les dates en PHP en convertissant d'abord les chaînes de date en timestamp Unix avec la fonction `strtotime()`, puis en les comparant. Cependant, cette méthode n'est pas fiable pour les dates avant 1970 ou loin dans le futur.

En alternative, vous pouvez utiliser des objets DateTime et la méthode `diff()`, comme nous l'avons vu précédemment, ou vous pouvez utiliser la méthode `DateTime::getTimestamp()` pour obtenir un timestamp Unix à partir d'un objet DateTime, qui peut être comparé comme n'importe quel autre nombre.

L'implémentation de la comparaison des dates en PHP est basée sur le modèle objet. En effet, `DateTime::diff()` renvoie un objet `DateInterval` qui représente la différence entre deux dates.

## Voir aussi :

Pour en savoir plus sur la comparaison des dates en PHP, jetez un œil à ces liens :

1. Documentation officielle PHP sur la classe DateTime : https://www.php.net/manual/fr/class.datetime.php
2. Documentation officielle PHP sur la classe DateInterval : https://www.php.net/manual/fr/class.dateinterval.php
3. Article de Stack Overflow sur la comparaison de deux dates en PHP : https://stackoverflow.com/questions/19402938/compare-two-dates-in-php