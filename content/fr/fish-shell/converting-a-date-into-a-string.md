---
title:                "Conversion d'une date en chaîne de caractères"
html_title:           "Fish Shell: Conversion d'une date en chaîne de caractères"
simple_title:         "Conversion d'une date en chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous utilisez Fish Shell, vous savez déjà que c'est un puissant environnement de ligne de commande avec un certain nombre de fonctionnalités intéressantes. Mais pourquoi pourriez-vous vouloir convertir une date en chaîne de caractères ? Eh bien, il existe plusieurs raisons possibles, notamment pour afficher une date formatée dans un script ou pour faciliter la lecture des dates dans votre terminal.

## Comment faire

Heureusement, Fish Shell a une commande intégrée pratique pour convertir une date en chaîne de caractères : `date`. Voyons comment l'utiliser pour obtenir différentes formats de dates :

```
Fish Shell >> date +%Y-%m-%d
2021-01-01
```

Dans cet exemple, nous utilisons l'option `%Y` pour obtenir l'année, `%m` pour le mois et `%d` pour le jour dans le format souhaité. Mais il existe une multitude d'autres options que vous pouvez utiliser pour obtenir différentes informations sur la date, telles que le jour de la semaine, l'heure, etc.

```
Fish Shell >> date +%A
Tuesday
Fish Shell >> date +%H:%M:%S
12:30:00
```

Vous pouvez également spécifier une date spécifique en utilisant l'option `-d` avec le format `YYYY-MM-DD` :

```
Fish Shell >> date -d "2021-12-25" +%A
Saturday
```

## Plongée plus profonde

Si vous voulez vraiment comprendre comment fonctionne la conversion d'une date en chaîne de caractères dans Fish Shell, voici quelques informations supplémentaires. La commande `date` utilise le langage `strftime` pour convertir la date en une chaîne de caractères, et Fish Shell l'a intégré pour faciliter son utilisation.

Le format `strftime` est un peu différent de ce que nous avons vu jusqu'à présent. Au lieu d'utiliser `%A` pour obtenir le jour de la semaine, nous utilisons `%u` pour obtenir le numéro du jour dans la semaine (1 pour lundi, 7 pour dimanche). Vous pouvez trouver plus de détails sur les différentes options sur la page de manuel de `strftime`.

## Voir aussi

- La documentation officielle de `fish date`: https://fishshell.com/docs/current/cmds/date.html
- Page de manuel de `strftime` : https://man7.org/linux/man-pages/man3/strftime.3.html
- Plus d'informations sur Fish Shell : https://fishshell.com/docs/current/