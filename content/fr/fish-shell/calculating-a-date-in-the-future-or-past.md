---
title:                "Fish Shell: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou le passé peut être utile pour planifier des tâches ou des événements à l'avance ou pour vérifier des dates passées. Cela peut également être un moyen de mieux comprendre comment le temps s'écoule et de résoudre des problèmes mathématiques liés aux dates.

## Comment faire

Le Fish Shell offre plusieurs commandes utiles pour calculer des dates dans le futur ou le passé. Voici quelques exemples:

```Fish Shell
# Calculer la date dans 5 jours
date -d "+5 days"

# Calculer la date il y a 2 semaines
date -d "-2 weeks"

# Calculer la date dans 1 mois et 3 jours
date -d "+1 month +3 days"

# Calculer la date il y a 2 ans et 1 mois
date -d "-2 years -1 month"
```

La sortie sera affichée au format suivant: "Jour Mois Année Heure:Minute:Seconde". Par exemple: "16 juillet 2020 15:00:00".

## Plongée en profondeur

Le Fish Shell utilise la commande "date" du système d'exploitation pour effectuer ces calculs de dates. Vous pouvez trouver plus d'informations sur la façon dont cette commande fonctionne en utilisant la commande "man date" dans votre terminal. Vous pouvez également utiliser des options supplémentaires pour personnaliser la sortie, comme en spécifiant un fuseau horaire ou un format de date différent.

Il est également possible de combiner ces commandes avec d'autres pour créer des scripts plus complexes qui prennent en compte des variables telles que les jours ouvrables, les congés ou les événements spéciaux.

## Voir aussi

- [Comment calculer une date avec le Fish Shell](https://fishshell.com/docs/current/commands.html#date)
- [Documentation sur la commande "date"](https://www.gnu.org/software/coreutils/date)
- [Liste des formats de date disponibles](https://www.gnu.org/software/coreutils/manual/html_node/Date-conversion-specifiers.html)