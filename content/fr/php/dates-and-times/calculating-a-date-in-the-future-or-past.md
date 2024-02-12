---
title:                "Calcul d'une date future ou passée"
aliases:
- /fr/php/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:35.109107-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcul d'une date future ou passée"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date dans le futur ou le passé consiste à déterminer une date relative à un point de référence. Les programmeurs le font souvent pour des rappels, des échéances ou pour suivre des événements.

## Comment faire :
```php
<?php
$dateActuelle = new DateTime(); // Aujourd'hui
echo "Date actuelle : " . $dateActuelle->format('Y-m-d H:i:s') . "\n";

// Ajouter 10 jours
$dateFutur = clone $dateActuelle;
$dateFutur->modify('+10 days');
echo "Dans 10 jours : " . $dateFutur->format('Y-m-d H:i:s') . "\n";

// Retirer 30 jours
$datePasse = clone $dateActuelle;
$datePasse->modify('-30 days');
echo "Il y a 30 jours : " . $datePasse->format('Y-m-d H:i:s') . "\n";
?>
```

Sortie exemple :
```
Date actuelle : 2023-03-15 17:45:23
Dans 10 jours : 2023-03-25 17:45:23
Il y a 30 jours : 2023-02-13 17:45:23
```

## Exploration plus profonde :
Historiquement, PHP a utilisé la fonction `strtotime` pour manipuler les dates, mais depuis PHP 5.2.0, les classes `DateTime` et `DateTimeImmutable` sont préférées pour leur meilleure gestion des fuseaux horaires et leur interface orientée objet.

Alternatives : outre `DateTime`, on peut utiliser `DateInterval` pour ajouter ou soustraire des périodes spécifiques et `DatePeriod` pour itérer sur une période avec une certaine fréquence.

Concernant l'implémentation, lorsque l'on travaille avec `DateTime`, il faut être attentif aux fuseaux horaires (avec `DateTimeZone`) pour éviter des erreurs de calcul. Les méthodes `add` et `sub` de `DateTime` peuvent également être utilisées pour des modifications directes sans la syntaxe de chaîne de `modify`.

## Voir aussi :
- [Documentation PHP sur DateTime](https://www.php.net/manual/fr/class.datetime.php)
- [Documentation PHP sur DateInterval](https://www.php.net/manual/fr/class.dateinterval.php)
- [Documentation PHP sur les fuseaux horaires](https://www.php.net/manual/fr/timezones.php)
