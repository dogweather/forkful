---
title:                "Fish Shell: Calcul d'une date dans le futur ou le passé"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent utile de pouvoir calculer une date dans le futur ou le passé dans un script Fish Shell. Que ce soit pour planifier des tâches ou pour vérifier des dates d'échéance, cette fonctionnalité peut s'avérer très pratique.

## Comment faire

Pour calculer une date dans le futur ou le passé, nous pouvons utiliser la commande `date` intégrée à Fish Shell. Voici un exemple de code pour calculer la date exacte dans 30 jours :

```
Fish Shell - Calculer une date dans le futur

# On définit une variable avec la date actuelle
set today (date "+%Y-%m-%d")

# On utilise la commande date pour ajouter 30 jours
set future (date -d "$today + 30 days" "+%Y-%m-%d")

# On affiche le résultat
echo $future  # Output : 2020-05-12
```

De la même manière, pour calculer une date dans le passé, il suffit d'utiliser un nombre de jours négatif dans la commande `date` :

```
Fish Shell - Calculer une date dans le passé

# On définit une variable avec la date actuelle
set today (date "+%Y-%m-%d")

# On utilise la commande date pour soustraire 30 jours
set past (date -d "$today - 30 days" "+%Y-%m-%d")

# On affiche le résultat
echo $past  # Output : 2020-03-13
```

## Plongée en profondeur

La commande `date` en Fish Shell utilise la syntaxe GNU pour calculer les dates. Cela signifie que nous pouvons utiliser des combinaisons de jours/mois/années ou même des jours de la semaine pour calculer une date. Par exemple, si nous voulons obtenir la date exacte dans 2 semaines, nous pouvons utiliser :

```
Fish Shell - Calculer une date dans 2 semaines

# On définit une variable avec la date actuelle
set today (date "+%Y-%m-%d")

# On utilise la commande date avec la syntaxe GNU pour ajouter 2 semaines
set future (date -d "$today + 2 weeks" "+%Y-%m-%d")

# On affiche le résultat
echo $future  # Output : 2020-04-03
```

Vous pouvez également utiliser des combinaisons de jours/mois/années pour calculer une date spécifique. Par exemple, si nous voulons obtenir la date du 1er avril de l'année en cours, nous pouvons utiliser :

```
Fish Shell - Calculer la date du 1er avril

# On définit une variable avec l'année actuelle
set year (date "+%Y")

# On utilise la commande date avec la syntaxe GNU pour obtenir la date du 1er avril
set april (date -d "04/01/$year" "+%Y-%m-%d")

# On affiche le résultat
echo $april  # Output : 2020-04-01
```

## Voir aussi

- [Documentation complète de la commande date GNU](https://www.gnu.org/software/coreutils/manual/html_node/Examples-of-date.html)
- [Guide des commandes en Fish Shell](https://fishshell.com/docs/current/index.html)