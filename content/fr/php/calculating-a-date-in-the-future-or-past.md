---
title:                "Calculer une date dans le futur ou le passé."
html_title:           "PHP: Calculer une date dans le futur ou le passé."
simple_title:         "Calculer une date dans le futur ou le passé."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est courant en programmation de devoir calculer une date dans le futur ou dans le passé. Cela peut servir dans de nombreuses applications telles que la gestion d'événements, la planification ou la mise à jour de données. Dans cet article, nous allons découvrir comment réaliser ce type de calculs en PHP de manière simple et efficace.

## Comment Faire

Pour calculer une date dans le futur ou dans le passé en PHP, nous allons utiliser la fonction ```date()``` combinée avec la fonction ```strtotime()```. La fonction ```date()``` permet de retourner la date actuelle selon un format spécifié, tandis que la fonction ```strtotime()``` permet de convertir une chaîne de caractères en timestamp, c'est-à-dire en nombre de secondes écoulées depuis le 1er janvier 1970.

Par exemple, pour calculer la date dans 2 semaines à partir d'aujourd'hui, nous pouvons utiliser la fonction ```strtotime()``` avec le paramètre ```"+2 weeks"```, qui retournera un timestamp correspondant à cette date. Ensuite, en utilisant la fonction ```date()``` avec le format souhaité, nous pouvons afficher cette date de la manière qui nous convient.

Voici un exemple de code pour calculer la date dans 2 semaines à partir d'aujourd'hui et l'afficher au format jour/mois/année :

```
<?php
$date = strtotime("+2 weeks");
echo date("d/m/Y", $date);
?>
```

Ce code va afficher la date dans 2 semaines à partir d'aujourd'hui, par exemple : 28/05/2021.

Il est également possible de calculer une date en utilisant d'autres unités de temps telles que les jours, les mois, les années, les heures ou les minutes. Voici un exemple pour calculer la date dans 1 mois et 3 jours à partir d'aujourd'hui :

```
<?php
$date = strtotime("+1 month +3 days");
echo date("d/m/Y", $date);
?>
```

Cela va afficher la date dans 1 mois et 3 jours à partir d'aujourd'hui, par exemple : 12/06/2021.

## Deep Dive

La fonction ```strtotime()``` peut également être utilisée pour calculer une date à partir d'un timestamp donné. Dans ce cas, nous pouvons utiliser le deuxième paramètre de la fonction pour spécifier le timestamp de référence. Par exemple, si nous voulons calculer une date dans 5 jours à partir du 15/05/2021, nous pouvons utiliser la fonction comme ceci :

```
<?php
$date = strtotime("+5 days", strtotime("15/05/2021"));
echo date("d/m/Y", $date);
?>
```

Cela va afficher la date dans 5 jours à partir du 15/05/2021, soit le 20/05/2021.

Il est également important de noter que la fonction ```strtotime()``` peut traiter une grande variété de chaînes de caractères en plus de celles présentées dans cet article. Vous pouvez trouver plus d'informations sur les formats de dates reconnus en consultant la documentation PHP sur la fonction [strtotime()](https://www.php.net/manual/fr/function.strtotime.php).

## Voir Aussi

- [Documentation PHP sur la fonction date()](https://www.php.net/manual/fr/function.date.php)
- [Documentation PHP sur la fonction strtotime()](https://www.php.net/manual/fr/function.strtotime.php)