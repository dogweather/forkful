---
title:                "Comparer deux dates"
html_title:           "PHP: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 
Comparer deux dates en programmation consiste simplement à vérifier si l'une est antérieure, égale ou postérieure à l'autre. Les programmeurs le font pour effectuer des comparaisons dans leurs applications, telles que des réservations de vol ou des commandes de produits.

## Comment faire: 
Il existe plusieurs manières de comparer deux dates en PHP. L'une des façons les plus simples est d'utiliser la fonction ``` strtotime () ``` pour convertir les dates en timestamp, puis de les comparer en utilisant des opérateurs de comparaison tels que ```< ``` ou ```>```. Voici un exemple de code:

```
$date1 = '2020-05-01';
$date2 = '2020-05-05';

$timestamp1 = strtotime($date1);
// résultat: 1588300800

$timestamp2 = strtotime($date2);
// résultat: 1588627200

if ($timestamp1 < $timestamp2){
    echo "La date 1 est antérieure à la date 2";
}

// résultat: La date 1 est antérieure à la date 2
```

Une autre façon serait d'utiliser la classe ```DateTime``` en PHP, qui offre des méthodes pratiques pour comparer les dates. Voici un exemple de code:

```
$date1 = new DateTime('2020-05-01');
$date2 = new DateTime('2020-05-05');

if ($date1 < $date2){
    echo "La date 1 est antérieure à la date 2";
}

// résultat: La date 1 est antérieure à la date 2
```

## Plongée en profondeur:
De nos jours, les langages de programmation proposent plusieurs façons de représenter les dates et les heures. En PHP, les développeurs peuvent utiliser des fonctions telles que ```date()``` et ```strtotime()``` pour manipuler les dates et les convertir en différents formats. Cependant, l'utilisation de la classe ```DateTime``` est recommandée car elle gère automatiquement les différences de fuseau horaire et les années bissextiles. 

Il existe également des alternatives pour comparer les dates en utilisant des expressions régulières ou des opérations logiques telles que ```and```, ```or```, ```not```. Cependant, ces approches peuvent être plus complexes et moins efficaces que l'utilisation de fonctions intégrées telles que ```strtotime()``` ou la classe ```DateTime```.

## À voir également:
Pour en savoir plus sur les façons de manipuler et de comparer les dates en PHP, vous pouvez consulter la documentation officielle de PHP: https://www.php.net/manual/fr/datetime.compare.php 

Vous pouvez également utiliser des outils tiers tels que Carbon (https://carbon.nesbot.com/) pour faciliter la manipulation et la comparaison des dates en PHP.