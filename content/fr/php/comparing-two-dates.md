---
title:                "Comparaison de deux dates"
html_title:           "PHP: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut être utile de comparer deux dates dans un programme PHP pour effectuer des opérations telles que la vérification de la validité d'une date ou le calcul de la durée entre deux événements.

## Comment faire

Pour comparer deux dates en PHP, vous pouvez utiliser la fonction prédéfinie `strtotime()` pour convertir une chaîne de caractères représentant une date en timestamp et ensuite utiliser les opérateurs de comparaison (comme `>`, `<`, `==`) pour comparer les timestamps.

```PHP
$date1 = strtotime('2020-08-15');
$date2 = strtotime('2020-08-20');

if ($date1 < $date2) {
  echo "La date 1 est antérieure à la date 2.";
} else if ($date1 > $date2) {
  echo "La date 2 est antérieure à la date 1.";
} else {
  echo "Les deux dates sont identiques.";
}
```

La sortie de ce code sera "La date 1 est antérieure à la date 2."

## Deep Dive

Il est important de noter que la fonction `strtotime()` peut avoir des comportements imprévisibles avec des dates au format non standard ou dans certains fuseaux horaires. Il est donc recommandé d'utiliser la classe `DateTime` pour une manipulation plus précise des dates.

```PHP
$date1 = new DateTime('2020-08-15');
$date2 = new DateTime('2020-08-20');

if ($date1 < $date2) {
  echo "La date 1 est antérieure à la date 2.";
} else if ($date1 > $date2) {
  echo "La date 2 est antérieure à la date 1.";
} else {
  echo "Les deux dates sont identiques.";
}
```

La sortie de ce code sera également "La date 1 est antérieure à la date 2." Cependant, il est important de noter que la classe `DateTime` offre des méthodes pour une comparaison plus précise, comme `->diff()` pour calculer la différence entre deux dates en jours, heures, minutes, etc.

## Voir aussi

- [Fonction strtotime() sur le site de PHP](https://www.php.net/manual/fr/function.strtotime.php)
- [Documentation complète de la classe DateTime](https://www.php.net/manual/fr/class.datetime.php)