---
title:                "Obtenir la date actuelle"
date:                  2024-02-03T19:10:12.960837-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtenir la date actuelle"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Obtenir la date actuelle en PHP est une tâche fondamentale qui vous permet de récupérer et de manipuler la date et l'heure du système. Cela est crucial pour des fonctions telles que la journalisation, le marquage temporel des publications, la planification d'événements ou l'exécution d'opérations sensibles au temps dans vos applications.

## Comment faire :
### PHP Natif
La fonction intégrée `date()` de PHP est le moyen le plus direct d'obtenir la date actuelle. Vous pouvez formater la date de diverses manières en spécifiant le paramètre de format.

```php
echo date("Y-m-d"); // Sortie : 2023-04-01 (par exemple)
echo date("l, F j, Y"); // Sortie : Saturday, April 1, 2023
```

Pour obtenir la date et l'heure avec support du fuseau horaire, vous pouvez utiliser la classe `DateTime` avec `DateTimeZone`.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Sortie: 2023-04-01 12:00:00 (par exemple)
```

### Utilisation de Carbon (Une Bibliothèque Tierce Populaire)
[Carbon](https://carbon.nesbot.com/) est une extension simple de l'API pour `DateTime` qui fournit une manière plus propre et plus fluide de travailler avec les dates et les heures.

Tout d'abord, assurez-vous d'avoir Carbon installé via Composer :
```bash
composer require nesbot/carbon
```

Ensuite, vous pouvez l'utiliser pour obtenir la date actuelle :

```php
use Carbon\Carbon;

echo Carbon::now(); // Sortie : 2023-04-01 12:00:00 (par exemple, dans le format par défaut)
echo Carbon::now()->toDateString(); // Sortie : 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Sortie : Saturday, April 1, 2023
```

Carbon enrichit la gestion des dates et heures en PHP en ajoutant de la lisibilité et une abondance de fonctionnalités pour la manipulation, la comparaison et le formatage du temps.
