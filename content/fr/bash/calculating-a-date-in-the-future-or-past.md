---
title:                "Calcul d'une date dans le futur ou le passé"
html_title:           "Bash: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est? Pourquoi?

Calculer une date dans le futur ou dans le passé est l'action de déterminer une date en utilisant une date de référence et un nombre de jours à ajouter ou à soustraire. Les programmeurs utilisent cette tâche pour automatiser des processus tels que la planification de tâches, la gestion de base de données et la création de rapports.

## Comment faire:

Voici deux façons de calculer une date dans le futur ou dans le passé en utilisant Bash:

```Bash
# Ajouter 10 jours à la date actuelle
date -d "+10 days" 

# Soustraire 5 jours de la date actuelle
date -d "-5 days"
```

Output:
```
Jeudi 11 juin 2020 12:00:00 CST
Samedi 6 juin 2020 12:00:00 CST
```

## Plongée en profondeur:

1. Contexte historique: Le calcul de dates a été un défi pour les programmeurs, car chaque système d'exploitation ou langage de programmation avait sa propre façon de stocker et de manipuler les dates. Bash utilise une bibliothèque appelée GNU Date pour faciliter cette tâche.

2. Alternatives: En plus de la méthode présentée ci-dessus, vous pouvez également utiliser la commande ```strftime``` pour formater une date dans le futur ou dans le passé. Cependant, cela nécessite une connaissance plus approfondie des formats de date.

3. Détails de mise en œuvre: La commande ```date -d``` prend en paramètre une chaîne de caractères qui spécifie la quantité de temps à ajouter ou à soustraire de la date de référence. Elle peut gérer des unités de temps telles que les secondes, les minutes, les heures, les jours, les semaines, les mois et les années.

## Voir aussi:

Pour en savoir plus sur la commande ```date``` de Bash, consultez la documentation officielle: [GNU Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)