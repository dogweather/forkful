---
title:                "Bash: Conversion d'une date en chaîne de caractères"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaîne de caractères peut être utile dans de nombreuses situations, notamment pour afficher des informations de date dans un format spécifique ou pour faciliter la manipulation de dates dans un script Bash.

## Comment faire

Pour convertir une date en chaîne de caractères en Bash, il existe plusieurs options. La méthode la plus courante est d'utiliser la commande `date` avec l'option `+%Y%m%d` pour obtenir la date au format AAAAMMJJ. Par exemple:

```Bash
date +%Y%m%d
```

Cela affichera la date actuelle au format AAAAMMJJ (ex: 20210304).

Nous pouvons également utiliser l'option `+%Y %m %d` pour obtenir la date avec des espaces entre les éléments, ou l'option `+%D` pour le format MM/JJ/AAAA. Pour plus d'options de formatage de date, consultez la documentation de `date` en utilisant la commande `man date`.

Une autre méthode consiste à utiliser l'outil de manipulation de dates `dateutils`, disponible sur la plupart des distributions Linux. Avec cet outil, nous pouvons spécifier le format de sortie souhaité en utilisant l'option `-i`, par exemple:

```Bash
dateutils.dconv -i "%Y%m%d" now
```

Cette commande affichera également la date actuelle au format AAAAMMJJ.

## Plongée en profondeur

La conversion d'une date en chaîne de caractères peut sembler simple, mais il existe en réalité de nombreuses nuances et possibilités en fonction du langage de programmation et de la bibliothèque utilisée.

En Bash, par exemple, nous pouvons également utiliser la commande `printf` pour formater une date spécifique. Par exemple:

```Bash
printf "%(%Y/%m/%d)T" 2020-12-31
```

Cette commande affichera la date spécifique au format AAAA/MM/JJ (ex: 2020/12/31). Nous pouvons également spécifier des variables pour les éléments de la date, comme l'année ou le mois, pour une plus grande flexibilité dans le formatage.

Il est également possible de convertir la date en chaîne de caractères dans d'autres langages de programmation tels que Python, en utilisant des bibliothèques comme `datetime` ou `dateutil`.

## Voir aussi

- La documentation de `date` sur Unix.com
- La documentation de `dateutils`
- Les options de format de date dans Bash sur SS64.com