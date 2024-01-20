---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Conversion d'une date en chaîne de caractères avec Fish Shell

## Qu'est-ce que c'est et pourquoi?

La conversion d'une date en chaîne de caractères, c'est convertir une valeur de type date dans une représentation de texte. Les programmeurs le font pour formatter les dates différemment, ou pour les stocker dans une base de données ou un fichier.

## Comment faire:

#### L'obtention de la date actuelle:

Avec Fish Shell, vous pouvez obtenir la date et l'heure actuelles en utilisant la commande `date`:

```fish
> date
lundi 12 juillet 2021, 14:45:08 (UTC+0200)
```

#### Convertir une date en chaîne de caractères:

Vous pouvez formatter la date et l'heure avec `strftime` (string format time):

```fish
> date "+%d/%m/%Y"
12/07/2021
```

Le code `%d/%m/%Y` spécifie le format de la date: jour, mois, année.

## Plongée plus profonde:

Historiquement, la manipulation des dates et des chaînes de caractères a toujours été un défi pour les programmeurs. Heureusement, la plupart des langages de programmation modernes, y compris Fish Shell, disposent d'outils pour rendre ce processus plus facile.

Il y a d'autres manières de convertir une date en chaîne de caractères. Par exemple, avec la commande `set`:

```fish
> set -l mydate (date "+%d/%m/%Y")
> echo $mydate
12/07/2021
```

Et avec `printf` (print formatted):

```fish
> printf "%s\n" (date "+%d/%m/%Y")
12/07/2021
```

Il est important de noter que `strftime` est une implémentation de la norme C utilisée pour formater les dates et les heures.

## A voir aussi:

Pour plus d'informations sur la conversion d'une date en chaîne de caractères dans Fish Shell, consultez les ressources suivantes:

1. [Fish Shell Documentation](https://fishshell.com/docs/3.1/)
2. [Commandes Fish Shell](https://fishshell.com/docs/3.1/cmds.html)
3. [Syntaxe Fish Shell](https://fishshell.com/docs/3.1/syntax.html)
4. [Time and date in Fish Shell](https://jorgebucaran.github.io/fish-cookbook/recipes/date-time.html)