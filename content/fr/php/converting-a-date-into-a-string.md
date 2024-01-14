---
title:                "PHP: Transformer une date en chaîne de caractères"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous travaillez avec des dates dans votre code PHP, il est probable que vous ayez besoin à un moment donné de les afficher sous forme de chaîne de caractères. Cela peut être utile pour une multitude de raisons, comme l'affichage de la date à l'utilisateur final, la création de rapports ou encore pour faciliter la manipulation de ces dates dans votre code.

# Comment faire

Pour convertir une date en une chaîne de caractères, il suffit d'utiliser la fonction `date()` en lui passant en premier argument un format de date et en second argument, la date à convertir.

```PHP
<?php
$date = strtotime('2021-01-01');
echo date('d/m/Y', $date); // affichera 01/01/2021
```

Cette fonction prend en compte de nombreux formats de date (voir la documentation officielle pour la liste complète), mais vous pouvez également utiliser des formats personnalisés en utilisant des lettres spécifiques pour représenter les différents éléments de la date (ex. d pour le jour, m pour le mois, Y pour l'année).

# Plongée en profondeur

La fonction `date()` repose en réalité sur une autre fonction appelée `strftime()` qui prend en compte la langue locale du serveur. Cela signifie que si vous utilisez cette fonction sur un serveur français, les noms des mois ou des jours de la semaine seront affichés en français (par exemple, "janvier" au lieu de "January"). Cela peut être utile si votre application est multilingue.

# Voir aussi

- La documentation officielle pour la fonction `date()` : https://www.php.net/manual/fr/function.date.php
- La documentation officielle pour la fonction `strftime()` : https://www.php.net/manual/fr/function.strftime.php