---
title:                "Calculation d'une date dans le futur ou le passé"
html_title:           "Fish Shell: Calculation d'une date dans le futur ou le passé"
simple_title:         "Calculation d'une date dans le futur ou le passé"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Calculer une date dans le futur ou dans le passé est une pratique courante pour les programmeurs. Cela leur permet de manipuler facilement les dates dans leurs programmes et d'effectuer des calculs tels que l'ajout ou la soustraction de jours, mois ou années à une date donnée.

## Comment faire:
Voici un exemple simple pour calculer une date dans le futur en utilisant la dernière version de Fish Shell : 
```
Fish Shell (v3.1.2) ➜ date --date='next week'
lun. 31 août 2020 00:00:00 CEST
```

Pour calculer une date dans le passé, il suffit d'utiliser l'option `--date` avec une date antérieure à celle actuelle : 
```
Fish Shell (v3.1.2) ➜ date --date='last month'
mer. 1 juil. 2020 00:00:00 CEST
```

## Plongée en profondeur:
Au fil des années, la manipulation des dates a évolué et les programmeurs ont utilisé différentes méthodes pour calculer les dates dans le futur ou dans le passé. Avant l'avènement des shells modernes comme Bash ou Fish, les utilisateurs devaient utiliser des programmes tels que `dateutils` pour effectuer ces calculs. Cependant, avec l'intégration de l'option `--date` dans les shells, cette tâche est devenue beaucoup plus simple et intuitive.

Si vous n'utilisez pas Fish Shell, sachez que d'autres shells comme Bash, Zsh et Ksh offrent également l'option `--date` pour calculer une date spécifique.

## Voir aussi:
Pour plus d'informations sur la manipulation des dates dans Fish Shell, consultez la documentation officielle : https://fishshell.com/docs/current/cmds/date.html

Pour en savoir plus sur les différentes méthodes pour calculer les dates dans les shells, vous pouvez également consulter cet article intéressant : https://www.lifewire.com/bash-tutorials-4091007#difference-between-bash-shell-date-and-timestamp