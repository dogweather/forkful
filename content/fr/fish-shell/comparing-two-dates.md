---
title:                "Fish Shell: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous programmez en Fish Shell, vous avez peut-être déjà eu besoin de comparer deux dates. Cela peut servir à plusieurs fins, comme vérifier si une date est plus récente qu'une autre ou calculer la différence entre deux dates. Dans cet article, nous allons explorer comment comparer deux dates en utilisant Fish Shell.

## Comment faire

Pour comparer des dates en Fish Shell, vous pouvez utiliser la commande `date` suivie des options appropriées pour spécifier les dates à comparer. Par exemple, pour vérifier si une date est plus récente qu'une autre, vous pouvez utiliser les options `-s` et `-d` pour spécifier les dates en tant que chaînes de caractères.

````fish
date -s "2020-01-01" -d "2020-01-05"
````

Cette commande renverra la valeur `1` si la deuxième date est plus récente que la première, `0` si elles sont identiques et `-1` si elle est plus ancienne. Vous pouvez également utiliser la commande `date` pour calculer la différence entre deux dates en utilisant l'option `-d %j` pour obtenir le nombre de jours entre les deux dates.

````fish
date -d "2020-01-01" -d %j "2020-01-05"
````

Cela vous donnera un résultat de `4`, car il y a quatre jours entre ces deux dates. La commande `date` offre également de nombreuses autres options pour formater et comparer les dates. Consultez la documentation officielle de Fish Shell pour en savoir plus sur ces options.

## Plongée en profondeur

Pour comprendre comment la commande `date` fonctionne, il est utile de savoir qu'elle utilise la bibliothèque `dateutil` pour effectuer les calculs de dates. Connaître les concepts de base de cette bibliothèque peut vous aider à mieux comprendre comment comparer les dates en Fish Shell.

La bibliothèque `dateutil` utilise une représentation en secondes pour les dates, en comptant à partir de l'époque Unix (le 1er janvier 1970 à 00h00 UTC). Cela signifie que toutes les dates sont converties en secondes avant d'être comparées. La commande `date` utilise ensuite ces valeurs de secondes pour vérifier la différence entre les dates.

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/)
- [Documentation officielle de la bibliothèque dateutil](https://dateutil.readthedocs.io/en/stable/)