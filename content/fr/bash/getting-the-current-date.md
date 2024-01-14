---
title:                "Bash: Obtenir la date actuelle"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La date actuelle est une information importante pour tout programmeur, car elle permet de suivre et de planifier les tâches à exécuter. En Bash, il existe plusieurs façons de récupérer la date actuelle, en fonction de vos besoins. Dans cet article, nous allons vous montrer comment le faire facilement.

## Comment faire

Il existe plusieurs commandes Bash pour récupérer la date actuelle. La plus simple est la commande `date`, qui affiche la date et l'heure actuelle au format par défaut. Voici un exemple de code et de résultat :

```Bash
#!/bin/bash
now=$(date)
echo "Aujourd'hui nous sommes le $now."
```

Output :

```
Aujourd'hui nous sommes le ven. 26 mars 2021 11:23:45 CET.
```

Si vous souhaitez personnaliser le format de la date, vous pouvez utiliser des options spécifiques avec la commande `date`. Par exemple, si vous voulez juste afficher le jour, le mois et l'année, vous pouvez utiliser l'option `%d` pour le jour, `%m` pour le mois et `%Y` pour l'année. Voici un exemple de code et de résultat :

```Bash
#!/bin/bash
now=$(date +"%d/%m/%Y")
echo "Aujourd'hui nous sommes le $now."
```

Output :

```
Aujourd'hui nous sommes le 26/03/2021.
```

Si vous avez besoin de la date actuelle dans un format spécifique pour une tâche particulière, vous pouvez également utiliser la commande `echo` avec les options de formatage de date. Par exemple, si vous avez besoin de la date au format AAAAMMJJ pour des tâches de tri ou de comparaison, vous pouvez utiliser l'option `%Y%m%d` avec la commande `echo`. Voici un exemple de code et de résultat :

```Bash
#!/bin/bash
echo $(date +"%Y%m%d")
```

Output :

```
20210326
```

## Profondeur d'analyse

Maintenant que vous savez comment récupérer la date actuelle en Bash, il est important de comprendre comment cette date est calculée. La commande `date` utilise l'horloge système de votre ordinateur pour afficher la date et l'heure actuelle. Ceci est basé sur le nombre de secondes écoulées depuis le 1er janvier 1970, qui est connu sous le nom de "timestamp Unix". Vous pouvez obtenir également ce timestamp en utilisant l'option `%s` avec la commande `date`.

Il est également utile de savoir qu'en plus des options de formatage de date, la commande `date` peut également effectuer des calculs de date. Par exemple, si vous avez besoin de la date 3 jours à partir d'aujourd'hui, vous pouvez utiliser l'option `-d` et spécifier le nombre de jours à ajouter. Voici un exemple de code et de résultat :

```Bash
#!/bin/bash
echo $(date -d "3 days")
```

Output :

```
mer. 31 mars 2021 11:23:45 CET
```

## Voir aussi

- [Documentation officielle de la commande date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Article informatif sur les timestamps Unix](https://en.wikipedia.org/wiki/Unix_time)
- [Plus d'options de formatage avec la commande date](https://www.tldp.org/LDP/abs/html/datecalc.html)