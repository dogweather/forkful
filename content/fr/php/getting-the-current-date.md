---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:15:50.215185-07:00
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Obtenir la date courante en PHP signifie récupérer la date et l'heure actuelles du serveur où s'exécute le script. C’est essentiel pour des fonctionnalités comme les logs, les timestamps ou les fonctions de planification.

## Comment faire :

Pour obtenir la date et l'heure actuelles, on utilise la fonction `date()` avec `Y-m-d H:i:s` comme format classique.

```PHP
echo "Date et heure actuelles : " . date("Y-m-d H:i:s");
```

Sortie possible :

```
Date et heure actuelles : 2023-03-15 14:23:52
```

Pour afficher uniquement la date, on omet les détails de l'heure :

```PHP
echo "Aujourd'hui, nous sommes le " . date("Y-m-d");
```

Sortie possible :

```
Aujourd'hui, nous sommes le 2023-03-15
```

## Exploration approfondie

Historiquement, PHP a toujours fourni des outils pour manipuler les dates et les heures. La fonction `date()` est le moyen le plus simple et le plus direct de récupérer la date actuelle, mais elle n'est que la pointe de l'iceberg. PHP fournit également des classes comme `DateTime` et `DateTimeZone` pour une gestion plus robuste des dates, y compris les fuseaux horaires et les modifications de dates.

Alternativement, pour une approche orientée objet, utilisez l'objet `DateTime` :

```PHP
$date = new DateTime();
echo $date->format('Y-m-d H:i:s');
```

En termes d'implémentation, PHP utilise la configuration `date.timezone` du fichier `php.ini` pour gérer les fuseaux horaires si aucun n’est spécifié lors de l'utilisation de `date()` ou de `DateTime`.

## Voir aussi

- La documentation officielle de PHP sur la fonction `date()`: [php.net/manual/fr/function.date.php](https://www.php.net/manual/fr/function.date.php)
- Pour une compréhension complète des formats de date et heure : [php.net/manual/fr/datetime.format.php](https://www.php.net/manual/fr/datetime.format.php)
- Pour une gestion approfondie des dates et heures avec les objets `DateTime` et `DateTimeZone`: [php.net/manual/fr/class.datetime.php](https://www.php.net/manual/fr/class.datetime.php)
