---
title:                "Fish Shell: Transformer une date en chaîne de caractères"
simple_title:         "Transformer une date en chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un utilisateur de Fish Shell, vous savez probablement que toutes les informations de temps sont stockées en tant que nombres de secondes depuis le 1er janvier 1970. Cela peut sembler un peu compliqué, mais cela a en fait de nombreux avantages. Cependant, dans certaines situations, vous pourriez avoir besoin de convertir ce nombre en une représentation plus facile à comprendre, comme une chaîne de caractères. Dans cet article, nous allons vous montrer comment le faire en utilisant Fish Shell.

## Comment faire

Converter un nombre de secondes en une chaîne de caractères en Fish Shell est en fait assez simple. Tout ce que vous devez faire est d'utiliser la commande `date` et spécifier le format souhaité en utilisant le drapeau `-f`. Par exemple, pour convertir le temps actuel en une chaîne de caractères avec le format "année-mois-jour heure:minutes:secondes", vous pouvez utiliser la commande suivante :

```
Fish Shell
date -f "%Y-%m-%d %H:%M:%S"
```

Cela renverra une sortie similaire à ceci :

```
2021-08-14 12:30:45
```

Vous pouvez également spécifier un temps spécifique à convertir en utilisant la commande `date -d`, qui prend une chaîne de caractères en tant que paramètre pour spécifier la date et l'heure souhaitées.

## Deep Dive

Il peut être utile de comprendre un peu plus en détail comment fonctionne cette conversion de nombre en une chaîne de caractères. En utilisant le drapeau `-f`, vous spécifiez un modèle de formatage qui sera utilisé pour la conversion. Ce modèle consiste en des caractères spéciaux qui représentent différentes parties du temps, comme l'année, le mois, le jour, l'heure, etc. Par exemple, `%Y` représente l'année complète à 4 chiffres, `%m` représente le mois en nombres, etc.

Il est également possible d'inclure des caractères différents des spécificateurs de formatage dans le modèle, qui seront simplement copiés dans la sortie finale. Par exemple, vous pouvez ajouter des tirets ou des points pour séparer les différentes parties de la date.

## Voir aussi

- [Commande date Fish Shell](https://fishshell.com/docs/current/cmds/date.html)
- [Liste des spécificateurs de formatage pour la commande date](https://fishshell.com/docs/current/cmds/date.html#format-specifiers)