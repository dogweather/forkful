---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Obtenir la date actuelle dans un programme Bash vous permet de l'insérer dans des scripts pour enregistrer des horodatages, programmer des tâches, fournir des informations contextuelles, etc. Ces informations sont souvent vitales pour le débogage et le suivi.

## Comment faire :
Le moyen le plus simple d'obtenir la date actuelle dans Bash est d'utiliser la commande `date`. Voici comment:
```Bash
#!/bin/bash
echo "La date d'aujourd'hui est:"
date +"%d-%m-%Y"
```
L'output sera par exemple:
```Bash
La date d'aujourd'hui est:
29-04-2021
```
## Exploration approfondie
La commande `date` est empruntée à Unix et est présente dans chaque distribution Linux, rendant son utilisation universelle. Pour davantage de flexibilité, vous pouvez également utiliser la commande `printf` avec `date`. Par exemple, pour les horodatages:
```Bash
#!/bin/bash
printf "Nous sommes le %s\n" "$(date)"
```
Ce code produit un horodatage complet, y compris le jour de la semaine, le mois, le jour du mois, l'heure, la minute, la seconde et l'année.

## Voir aussi
Regardez les pages de manuel pour plus d'informations et d'options pour la commande `date`: [Man Page for date](https://www.man7.org/linux/man-pages/man1/date.1.html)
ou pour approfondir dans la gestion du temps en bash script voir ce lien : [Time Management in Bash](https://www.tutorialkart.com/bash-shell-scripting/bash-date-time/)