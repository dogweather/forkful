---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Fish Shell: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi
Calculer une date future ou passée peut être utile dans de nombreuses situations, comme planifier des événements, gérer des tâches ou simplement suivre le temps qui passe.

## Comment faire
Pour effectuer ces calculs de dates dans Fish Shell, vous pouvez utiliser la commande `date` combinée avec des opérateurs mathématiques et des variables. Par exemple, pour calculer la date dans 3 jours à partir d'aujourd'hui, vous pouvez taper la commande suivante :

```Fish Shell
date -d "now + 3 days"
```

Et voici ce que vous obtiendrez comme résultat :

```
mer. 01 déc. 2021 08:21:10 CET
```

Vous pouvez également spécifier une date de départ à partir de laquelle calculer, en utilisant le format `yyyy-mm-jj`. Par exemple, pour calculer la date dans 1 an et 5 mois à partir du 15 février 2019, vous pouvez utiliser la commande suivante :

```Fish Shell
date -d "2019-02-15 + 1 year + 5 months"
```

Et voici le résultat :

```
dim. 15 juil. 2020 08:21:10 CEST
```

## Plongée en profondeur
La commande `date` utilise la bibliothèque Python `dateutil` pour effectuer les calculs de dates. Vous pouvez donc également utiliser les méthodes de cette bibliothèque directement dans Fish Shell pour des calculs plus avancés. Consultez la documentation de `dateutil` pour plus d'informations sur les fonctions disponibles.

## Voir aussi
- [Documentation de la commande date](https://fishshell.com/docs/current/commands.html#date)
- [Documentation de la bibliothèque dateutil](https://dateutil.readthedocs.io/en/stable/)