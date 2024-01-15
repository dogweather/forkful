---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Bash: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou dans le passé peut être utile pour planifier des événements ou pour vérifier des dates importantes. Cela peut également être utile pour des tâches de gestion de projet ou tout simplement pour satisfaire sa curiosité.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en utilisant Bash, vous pouvez utiliser la commande `date` en spécifiant la date et le format que vous souhaitez obtenir. Voici quelques exemples :

```Bash
# Calculer la date d'aujourd'hui
date +"%d/%m/%Y"

# Calculer la date dans 7 jours
date --date="+7 days" +"%d/%m/%Y"

# Calculer la date dans 1 mois
date --date="+1 month" +"%d/%m/%Y"

# Calculer la date dans 5 ans
date --date="+5 years" +"%d/%m/%Y"

# Calculer la date il y a 2 semaines
date --date="-2 weeks" +"%d/%m/%Y"

# Calculer la date il y a 1 an
date --date="-1 year" +"%d/%m/%Y"
```

Output :

```
05/08/2021
12/08/2021
05/09/2021
05/08/2026
22/07/2021
05/08/2020
```

## Plongée profonde

La commande `date` peut être utilisée pour calculer une date en utilisant différentes unités de temps telles que les jours, les semaines, les mois et les années. Elle peut également être utilisée pour afficher la date dans un format spécifique en utilisant les spécificateurs de format, tels que `%d` pour le jour, `%m` pour le mois et `%Y` pour l'année. En utilisant la commande `date` avec les options appropriées, vous pouvez facilement calculer des dates dans le futur ou dans le passé.

## Voir aussi

- [Documentation de la commande `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Calculer des dates en utilisant la commande `date`](https://www.computerhope.com/unix/udate.htm)