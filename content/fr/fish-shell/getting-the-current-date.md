---
title:                "Fish Shell: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La date actuelle est une information importante à connaître dans de nombreux contextes de programmation. Que ce soit pour afficher la date dans votre interface utilisateur ou pour effectuer des tâches basées sur la date, il est essentiel de savoir comment l'obtenir dans votre code. Dans cet article, nous allons découvrir comment obtenir la date actuelle en utilisant le shell Fish.

## Comment faire

La manière la plus simple d'obtenir la date actuelle dans Fish Shell est d'utiliser la commande `date`. Elle affichera la date et l'heure actuelles dans votre fuseau horaire local par défaut.

```Fish Shell
date
```

L'exécution de cette commande affichera un résultat similaire à celui-ci :

```
Mercredi 9 septembre 2020 17:30:00 Heure avancée du Pacifique (GMT-7)
```

Si vous souhaitez afficher la date et l'heure dans un format spécifique, vous pouvez spécifier un format en utilisant l'option `-f`. Par exemple, pour afficher la date au format ISO 8601, vous pouvez utiliser la commande suivante :

```Fish Shell
date -f "%Y-%m-%d"
```

Le résultat sera alors :

```
2020-09-09
```

Vous pouvez également spécifier un fuseau horaire différent en utilisant l'option `-u` suivie du fuseau horaire souhaité. Par exemple, pour avoir la date et l'heure en heure universelle (UTC), vous pouvez utiliser la commande suivante :

```Fish Shell
date -u
```

Le résultat sera alors :

```
Jeudi 10 septembre 2020 00:30:00 Temps universel (UTC)
```

## Plongée en profondeur

La commande `date` utilise en fait les variables d'environnement `LC_TIME` et `TZ` pour afficher la date et l'heure. La variable `LC_TIME` définit le format de la date et de l'heure, tandis que `TZ` définit le fuseau horaire.

La commande `date` utilise également l'utilitaire système `date` pour effectuer la tâche réelle d'obtenir la date. Cela signifie que si vous avez besoin de fonctionnalités plus avancées, vous pouvez utiliser directement l'utilitaire `date` en appelant `/bin/date`.

## Voir aussi
- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/)
- [La documentation officielle de la commande date](https://fishshell.com/docs/current/cmds/date.html)
- [La documentation de l'utilitaire système date](https://man7.org/linux/man-pages/man1/date.1.html)