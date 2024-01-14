---
title:                "Bash: Calculer une date dans le futur ou le passé."
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles quelqu'un pourrait avoir besoin de calculer une date dans le futur ou dans le passé en programmation Bash. Cela peut être utile pour planifier des tâches à exécuter à une date précise, ou pour effectuer des opérations sur une plage de dates.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en Bash, il existe plusieurs méthodes. Nous allons voir ici deux façons de le faire : en utilisant les commandes date et expr, ou en utilisant les fonctions internes à Bash.

```
Bash code block example:
# Calculer une date dans le futur en utilisant la commande date
date -d "1 day" # renvoie la date dans un jour
date -d "1 week" # renvoie la date dans une semaine

# Calculer une date dans le futur en utilisant la commande expr
# On peut utiliser les opérateurs + et - pour ajouter ou soustraire des jours/mois/années
expr $(date +%s) + 86400 # renvoie la date dans un jour
expr $(date +%s) + 604800 # renvoie la date dans une semaine

# Utiliser les fonctions internes à Bash pour calculer une date
# On peut utiliser le format de date "secondes depuis l'époque Unix" pour effectuer les opérations
# sur les dates.
# Voici une fonction pour calculer une date dans le futur ou dans le passé en utilisant une échéance de jours :
calc_date(){
local add=$1 # le nombre de jours à ajouter ou soustraire
local epoch=$(date -d "midnight" +"%s") # récupère les secondes depuis l'époque Unix
local target_date=$(expr $epoch + 86400 \* $add) # calcule la date cible en ajoutant ou soustrayant les jours
date -d @$target_date # affiche la date cible dans le format voulu
}
calc_date 1 # renvoie la date dans un jour
calc_date -7 # renvoie la date il y a une semaine
```

## Plongée en profondeur

Pour calculer une date dans le futur ou dans le passé, il est important de comprendre comment Bash gère les dates et les heures. Bash utilise l'époque Unix, qui est le 1er janvier 1970 à 00h00 UTC, comme point de départ pour compter les secondes depuis cette date. Ainsi, en utilisant les opérateurs mathématiques, nous pouvons ajouter ou soustraire des secondes pour calculer une nouvelle date.

Il est également important de noter que les commandes date et expr peuvent différer selon le système d'exploitation utilisé. Il est donc recommandé de vérifier la documentation du système d'exploitation pour les détails spécifiques.

## Voir aussi

- [Calculer la date d'aujourd'hui en Bash](https://www.millamint.fr/blog/calculer_date_bash/)
- [Documentation date command](https://www.gnu.org/software/coreutils/manual/html_node/Examples-of-date.html#Examples-of-date)
- [Documentation expr command](https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html#expr-invocation)