---
date: 2024-01-20 17:36:57.147731-07:00
description: "How to: (Comment faire:) Pour convertir une date en cha\xEEne de caract\xE8\
  res en PHP, utilisez la fonction `date_format()` ou la m\xE9thode `format` d'un\
  \ objet\u2026"
lastmod: '2024-04-05T21:53:59.376210-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire:) Pour convertir une date en cha\xEEne de caract\xE8res en\
  \ PHP, utilisez la fonction `date_format()` ou la m\xE9thode `format` d'un objet\
  \ `DateTime`."
title: "Conversion d'une date en cha\xEEne de caract\xE8res"
weight: 28
---

## How to: (Comment faire:)
Pour convertir une date en chaîne de caractères en PHP, utilisez la fonction `date_format()` ou la méthode `format` d'un objet `DateTime`. Voici quelques exemples :

```PHP
<?php
$date = new DateTime('now');
echo $date->format('Y-m-d H:i:s');  // Format standard ISO 8601
// Affichera, par exemple, "2023-03-15 13:45:12"

// Autre façon avec la fonction date()
echo date('d/m/Y');  // Format jour/mois/année
// Affichera la date d'aujourd'hui, par exemple, "15/03/2023"
?>
```

## Deep Dive (Plongée en profondeur)
Historiquement, PHP gère les dates avec la fonction `date()` introduite dans les premières versions. Plus récemment, l'objet `DateTime` a été introduit, apportant plus de flexibilité. Il existe plusieurs alternatives pour manipuler des dates en PHP :

1. `DateTime` et `DateTimeImmutable` pour une approche orientée objet.
2. `strftime()`, qui utilise les formats basés sur les paramètres régionaux (peu recommandé car déprécié).
3. Fonctions de l'extension `intl` pour une meilleure internationalisation.

Une implémentation dépendra de vos besoins de formatage, de modification de fuseaux horaires, ou de calcul de dates. Par exemple, utiliser `DateTime` est recommandé pour un travail plus complexe car il offre des méthodes pour l'addition et la soustraction, la comparaison, et la modification de fuseaux horaires.

## See Also (Voir également)
- Documentation officielle PHP sur DateTime: [php.net/manual/fr/class.datetime.php](https://www.php.net/manual/fr/class.datetime.php)
- Formats de date et d'heure PHP: [php.net/manual/fr/function.date.php](https://www.php.net/manual/fr/function.date.php)
- Extension intl pour l'internationalisation: [php.net/manual/fr/book.intl.php](https://www.php.net/manual/fr/book.intl.php)
