---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:16.281463-07:00
description: "Analyser une date \xE0 partir d'une cha\xEEne en PHP implique de convertir\
  \ un texte repr\xE9sentant une date et/ou une heure en un objet `DateTime` de PHP\
  \ ou en\u2026"
lastmod: '2024-02-25T18:49:54.608539-07:00'
model: gpt-4-0125-preview
summary: "Analyser une date \xE0 partir d'une cha\xEEne en PHP implique de convertir\
  \ un texte repr\xE9sentant une date et/ou une heure en un objet `DateTime` de PHP\
  \ ou en\u2026"
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Analyser une date à partir d'une chaîne en PHP implique de convertir un texte représentant une date et/ou une heure en un objet `DateTime` de PHP ou en d'autres formats de date/heure. Ceci est crucial pour la validation, la manipulation, le stockage et la présentation des données, surtout lorsqu'on travaille avec des entrées utilisateur ou des données issues de sources externes.

## Comment faire :

La classe intégrée `DateTime` de PHP offre un ensemble puissant de fonctions pour analyser et travailler avec les dates. Vous pouvez créer une instance `DateTime` à partir d'une chaîne de date en utilisant le constructeur, puis la formater selon les besoins. Voici comment :

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Sortie : 2023-04-25 15:30:00
```

Pour gérer des chaînes qui suivent des formats non standard, vous pouvez utiliser la méthode `createFromFormat`, qui vous permet de spécifier le format exact de la date d'entrée :

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Sortie : 2023-04-25 15:30:00
```

Pour une analyse plus complexe qui pourrait ne pas être directement prise en charge par `DateTime`, PHP propose la fonction `strtotime`, qui tente d'analyser toute description textuelle de date/heure en anglais en un horodatage Unix :

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// La sortie variera en fonction de la date actuelle, par ex., "2023-05-04"
```

**Utilisation des bibliothèques tierces :**

Bien que les fonctions intégrées de PHP couvrent une large gamme de cas d'utilisation, vous pourriez parfois avoir besoin de capacités d'analyse plus sophistiquées. La bibliothèque Carbon, une extension de la classe DateTime de PHP, offre un ensemble riche de fonctionnalités pour la manipulation de date/heure :

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// La sortie variera, par ex., "2023-04-26 00:00:00"
```

La méthode `parse` de Carbon peut gérer intelligemment une multitude de formats de date et d'heure, ce qui en fait un outil inestimable pour les applications nécessitant une fonctionnalité d'analyse de date flexible.
