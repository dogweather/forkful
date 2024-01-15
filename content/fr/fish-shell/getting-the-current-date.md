---
title:                "Obtenir la date actuelle"
html_title:           "Fish Shell: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Tu te demandes peut-être pourquoi tu devrais t'intéresser à obtenir la date actuelle dans ton programme Fish Shell. Eh bien, c'est parce que la date est une donnée couramment utilisée dans les scripts et les automatisations, en particulier pour les sauvegardes ou les tâches planifiées. Il est donc utile de savoir comment obtenir la date actuelle dans Fish Shell.

## Comment faire

Voici quelques exemples de code pour obtenir la date actuelle dans Fish Shell :

```Fish Shell
set current_date (date +%d-%m-%Y)
echo "La date actuelle est $current_date"
```

Ce code utilise la commande `date` avec l'option `%d-%m-%Y` pour formater la date en jour-mois-année, puis l'affiche avec la commande `echo`.

Tu peux également utiliser la commande `date` avec l'option `%A` pour obtenir le jour de la semaine :

```Fish Shell
set current_day (date +%A)
echo "Aujourd'hui, c'est $current_day"
```

Si tu as besoin d'obtenir la date et l'heure, tu peux utiliser l'option `+%d-%m-%Y_%H:%M:%S` :

```Fish Shell
set current_datetime (date +%d-%m-%Y_%H:%M:%S)
echo "La date et l'heure actuelles sont $current_datetime"
```

## Plongée en profondeur

La commande `date` a de nombreuses autres options disponibles. Tu peux taper `man date` dans ton terminal pour voir toutes les options et leur utilisation.

Tu peux également utiliser des options supplémentaires dans ta commande `echo` pour formater la sortie de la date selon tes préférences. Par exemple, `echo (date +%x)` affichera la date sous la forme "JJ/MM/AAAA".

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guide de démarrage rapide de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Commandement date dans Fish Shell](https://fishshell.com/docs/current/commands.html#date)