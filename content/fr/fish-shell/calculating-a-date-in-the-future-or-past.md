---
title:    "Fish Shell: Calculer une date dans le futur ou le passé"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez probablement pourquoi calculer une date dans le futur ou dans le passé avec Fish Shell. Eh bien, cela peut être utile pour planifier des événements ou des tâches à l'avance, ou pour rechercher des dates spécifiques dans le passé.

## Comment faire

Calculer une date dans le futur ou dans le passé avec Fish Shell est assez simple. Utilisez la commande ```date -v [unité de temps] [nombre]```, en remplaçant "[unité de temps]" par une des options suivantes: 
- s (secondes)
- m (minutes)
- h (heures)
- d (jours)
- w (semaines)
- M (mois)
- y (années)

Et en remplaçant "[nombre]" par la quantité de temps que vous souhaitez ajouter (pour une date future) ou soustraire (pour une date dans le passé). Par exemple, si vous voulez calculer la date d'aujourd'hui plus 5 jours, vous pouvez utiliser la commande ```date -v d 5```. 

Voici un exemple de sortie pour cette commande:
```
vendredi 13 décembre 2019, 12:00:00 (UTC+0100)
```

Si vous voulez une date spécifique dans le futur ou dans le passé, vous pouvez utiliser la commande ```date -v [unité de temps] [nombre] [date de référence]```. Par exemple, si vous voulez savoir quelle était la date il y a 8 jours à partir du 1er janvier 2020, vous pouvez utiliser la commande ```date -v d -8 2020-01-01```.

Voici un exemple de sortie pour cette commande:
```
mercredi 25 décembre 2019, 12:00:00 (UTC+0100)
```

## Plongée Profonde

Si vous voulez aller plus loin, vous pouvez également utiliser la commande ```date -d [date] [opération]```. Cela vous permet de faire des opérations mathématiques directement sur une date donnée. Par exemple, si vous voulez savoir quelle était la date il y a exactement un an, vous pouvez utiliser la commande ```date -d "20200202 + 1 year"```.

Voici un exemple de sortie pour cette commande:
```
mardi 2 février 2021, 12:00:00 (UTC+0100)
```

Il est également possible d'utiliser des informations plus précises sur la date, telles que l'heure et les secondes, pour effectuer des opérations. Consultez la [documentation de Fish Shell](https://fishshell.com/docs/current/cmds/date.html) pour plus d'exemples et de détails sur ces commandes.

## Voir Aussi

- [Documentation de Fish Shell sur la commande *date*](https://fishshell.com/docs/current/cmds/date.html)
- [Date Calculator Tool pour Fish Shell](https://github.com/mr6r4y/fish-date-calculator)
- [Article sur les calculs de dates avec Fish Shell](https://medium.com/@Erik_Goldstein/calculate-dates-in-the-future-and-past-with-fish-shell-6d1dd74764c7)