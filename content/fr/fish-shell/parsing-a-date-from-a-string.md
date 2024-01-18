---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
html_title:           "Fish Shell: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

Parser une date à partir d'une chaîne de caractères, c'est extraire la partie de la chaîne qui correspond à la date et la convertir en un format utilisable pour les programmes. Les programmeurs font cela pour gérer les dates dans leurs applications, telles que les calendriers, les systèmes de réservation et les bases de données.

## Comment faire:

```Fish Shell``` facilite le parsing des dates à partir de chaînes de caractères avec la fonction `date`. Par exemple, pour parser la date "25 janvier 2022" à partir d'une chaîne, on peut utiliser `date -f "%d %B %Y" "25 janvier 2022"` pour obtenir la sortie suivante: `22-01-25T00:00:00`.

Pour un format de date plus complexe, on peut utiliser `strptime` pour spécifier le format et la chaîne à parser. Par exemple, `strptime "%d-%m-%Y" "30-11-2021"` retournera `2021-11-30T00:00:00` en utilisant le format de date ISO.

## Plongée en profondeur:

Parser les dates à partir de chaînes de caractères a été un problème récurrent pour les programmeurs depuis les débuts de l'informatique. Les alternatives incluent l'utilisation de bibliothèques externes comme `dateutils` ou `moment.js`, mais l'intégration de ces bibliothèques peut être fastidieuse. C'est pourquoi `Fish Shell` rend le parsing des dates aussi simple que possible en fournissant des fonctions natives telles que `date` et `strptime`.

La mise en œuvre de la fonction `date` dans `Fish Shell` repose sur la bibliothèque de date standard de votre système, tandis que `strptime` utilise le format de date de la bibliothèque `strftime`. Cela permet une compatibilité élevée avec les différents systèmes et formats de date.

## Voir aussi:

- [Documentation officielle de `Fish Shell`](https://fishshell.com/docs/current/cmds/date.html)
- [Exemples de parsing de dates avec `Fish Shell`](https://www.mankier.com/1/date)
- [Utilisation de `strptime` avec `Fish Shell`](https://www.mankier.com/1/strptime)