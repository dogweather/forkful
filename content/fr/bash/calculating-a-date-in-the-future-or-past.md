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

## Qu'est-ce que & Pourquoi ?

Calculer une date dans le futur ou le passé, c'est obtenir une date qui est un certain nombre de jours, semaines, mois ou années à partir d'une date donnée. Les programmeurs le font généralement pour des planifications, des rappels ou des comparaisons de dates.

## Comment faire:

Voici comment calculer une date dans le futur ou le passé en Bash:

```Bash
# Pour obtenir la date de demain
date -d "+1 day"

# Pour obtenir la date d'hier
date -d "1 day ago"

# Pour obtenir la date d'il y a une semaine
date -d "1 week ago"

# Pour obtenir la date d'un mois plus tard
date -d "+1 month"

# Sortie d'exemple pour "+1 day"
Mon Mar 15 01:45:50 PDT 2022
```

Pour la plupart des usages, `-d` string ou `--date=STRING` suffit pour décrire la date recherchée.

## Plongée en profondeur

Historiquement, les opérations sur les dates n'étaient pas prises en charge nativement par Bash, mais elles ont été ajoutées plus tard à `date` (un utilitaire standard Unix). Aujoud'hui, presque tous les systèmes Unix-like ont cet utilitaire.

Une alternative à utiliser `date` en Bash pour calculer une date dans le futur ou le passé est l'utilisation de la librairie `datetime` en Python, ou la fonction `strtotime()` en PHP.

Dans Bash, lorsque vous fournissez un argument à `date -d`, il est analysé pour déterminer l'intervalle de temps à ajouter ou à soustraire à la date actuelle. Les espaces sont nécessaires entre le nombre et l'unité de temps (par exemple, `1 day`, `1 week`, `1 month`).

## Pour aller plus loin

Pour en savoir plus sur `date` et les autres utilitaires de calcul de date en programmation, consultez les ressources suivantes :

1. `date` man page: man7.org/linux/man-pages/man1/date.1.html
2. GNU Coreutils: www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
3. Stack Overflow - Calculate date/time difference in bash : stackoverflow.com/questions/6455050/calculate-date-time-difference-in-bash
4. UNIX & Linux Stack Exchange - How to add days to date in bash? unix.stackexchange.com/questions/97925/how-to-add-days-to-date-in-bash