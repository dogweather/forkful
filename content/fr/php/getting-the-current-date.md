---
title:                "Obtenir la date actuelle"
html_title:           "PHP: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle dans un script PHP peut sembler une tâche simple, mais elle est essentielle pour de nombreuses applications. Que ce soit pour afficher la date sur un site web, enregistrer la date d'un événement ou effectuer une analyse statistique, la récupération de la date actuelle est une fonctionnalité courante et importante dans la programmation en PHP.

## Comment faire

Pour obtenir la date actuelle en PHP, il existe plusieurs méthodes. Nous allons en explorer trois en utilisant la fonction `date()`, la classe `DateTime` et l'extension `DateTimeImmutable`.

- Utiliser la fonction `date()`:

```PHP
<?php
$date = date("d/m/Y"); //Récupère la date sous le format jour/mois/année
echo $date; //Affiche la date actuelle
```

Résultat:

```
14/02/2021
```

- Utiliser la classe `DateTime`:

```PHP
<?php
$date = new DateTime(); //Crée un nouvel objet DateTime avec la date et l'heure actuelles
echo $date->format('d/m/Y'); //Formatte la date selon le format jour/mois/année
```

Résultat:

```
14/02/2021
```

- Utiliser l'extension `DateTimeImmutable` (disponible depuis PHP 5.5):

```PHP
<?php
$date = DateTimeImmutable::createFromFormat('d/m/Y', '14/02/2021'); //Crée un nouvel objet DateTimeImmutable avec la date spécifiée
echo $date->format('d/m/Y'); //Formatte la date selon le format jour/mois/année
```

Résultat:

```
14/02/2021
```

## Deep Dive

En utilisant la fonction `date()`, il est possible de spécifier différents formats pour la date en utilisant des codes spéciaux. Par exemple, `d` représente le jour, `m` représente le mois et `Y` représente l'année. Vous pouvez également ajouter d'autres caractères pour séparer les valeurs, comme `/` ou `-`. Pour une liste complète des codes disponibles, vous pouvez consulter la documentation officielle de PHP.

Avec la classe `DateTime`, vous pouvez utiliser les méthodes `modify()` et `add()` pour ajuster la date selon vos besoins. Par exemple, pour ajouter un jour à la date actuelle, vous pouvez utiliser ` $date->modify('+1 day');` ou ` $date->add(new DateInterval('P1D'));`.

Quant à l'extension `DateTimeImmutable`, elle permet de créer des objets DateTime qui peuvent être utilisés en toute sécurité sans risque de les modifier accidentellement.

## Voir aussi

- Documentation officielle de PHP sur `date()`: https://www.php.net/manual/fr/function.date.php
- Documentation officielle de PHP sur `DateTime`: https://www.php.net/manual/fr/class.datetime.php
- Documentation officielle de PHP sur `DateTimeImmutable`: https://www.php.net/manual/fr/class.datetimeimmutable.php