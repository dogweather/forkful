---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:37:42.952022-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Parsez une date depuis une chaîne de caractères, c'est transformer le texte en une date que PHP peut comprendre et manipuler. On fait ça pour stocker, comparer ou modifier des dates de manière plus intuitive.

## Comment faire :
```php
<?php
// Exemple de parsing de date
$dateString = '2023-03-15 16:00:00';
$dateObject = DateTime::createFromFormat('Y-m-d H:i:s', $dateString);

// Output la date parseé
echo $dateObject->format('d/m/Y H:i:s') . "\n"; // 15/03/2023 16:00:00

// Parse une date avec une timezone spécifique
$timezone = new DateTimeZone('Europe/Paris');
$dateWithTimezone = DateTime::createFromFormat('Y-m-d H:i:s', $dateString, $timezone);

// Output avec timezone
echo $dateWithTimezone->format('d/m/Y H:i:s T') . "\n"; // 15/03/2023 16:00:00 CET
?>
```

## Exploration approfondie
Historiquement, PHP a utilisé la fonction `strtotime()` pour traduire des textes anglais en timestamps Unix. Avec l'introduction des objets `DateTime` en PHP 5.2.0, la manipulation des dates est devenue plus puissante et flexible.

Il existe d'autres alternatives, comme `IntlDateFormatter` pour des formats internationaux ou `strptime()` pour parser selon des formats spécifiés. Côté implémentation, `DateTime::createFromFormat()` échoue silencieusement, renvoyant `false` en cas de problème, alors il faut toujours vérifier les erreurs avec `DateTime::getLastErrors()`.

## À voir également
- Documentation officielle de PHP sur `DateTime`: https://www.php.net/manual/fr/class.datetime.php
- Fonction `strtotime()`: https://www.php.net/manual/fr/function.strtotime.php
- Classe `DateTimeZone`: https://www.php.net/manual/fr/class.datetimezone.php
- `IntlDateFormatter`: https://www.php.net/manual/fr/class.intldateformatter.php
