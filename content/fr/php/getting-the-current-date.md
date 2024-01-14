---
title:                "PHP: Obtenir la date actuelle"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi
La fonctionnalité de récupération de la date actuelle peut sembler simple, mais elle est cruciale pour de nombreux projets de programmation PHP. Elle permet aux développeurs de créer des scripts dynamiques basés sur le temps et la date, tels que des calendriers, des rappels ou des fonctionnalités de suivi de l'utilisation d'une application.

## Comment faire
Pour récupérer la date actuelle en PHP, il suffit d'utiliser la fonction intégrée `date()` suivie d'un paramètre spécifié pour le format de la date souhaitée. Par exemple, pour obtenir la date complète au format "jour/mois/année", on peut utiliser `date('d/m/Y')`. Voici un exemple concret :

```PHP
<?php
echo "Nous sommes le " . date('d/m/Y') . " et il est " . date('H:i') . ".";
```

Cela donnera en sortie : "Nous sommes le 15/05/2021 et il est 14:30."

## Plongée en profondeur
La fonction `date()` prend en paramètres différents formats qui peuvent être utilisés pour obtenir des informations spécifiques sur la date et l'heure (par exemple `d` pour le jour du mois, `m` pour le mois, `Y` pour l'année). Elle peut également prendre en compte la localisation en ajoutant le paramètre `setlocale()` avant la fonction `date()`. Il existe également d'autres fonctions et méthodes pour manipuler et formater les dates en PHP, telles que `strtotime()` et `DateTime`. Il est important de comprendre ces concepts pour gérer efficacement les dates dans un projet de programmation.

## Voir aussi
- [Documentation officielle de la fonction date() de PHP](https://www.php.net/manual/en/function.date.php)
- [Manipulation des dates et heures en PHP](https://www.php.net/manual/en/datetime.formats.php)
- [Tutoriel sur la gestion des dates en PHP](https://www.tutorialrepublic.com/php-tutorial/php-date-and-time.php)