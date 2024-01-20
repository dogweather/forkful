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

## Quoi & Pourquoi?
Calculer une date dans le futur ou le passé est une technique pour déterminer une date spécifique à partir d'une date de référence donnée. Les programmeurs s'en servent généralement pour automatiser des tâches basées sur le temps ou pour analyser des données historiques.

## Comment faire:
Calculez une date future en utilisant la commande `date` de Fish Shell avec l'option `-v` pour ajuster la valeur. Par exemple, pour obtenir la date de demain:

```Fish Shell
set date (date -v+1d)
echo $date
```

Pour une date passée, changez simplement le signe `+` en `-`. Par exemple, pour la date d'hier:

```Fish Shell
set date (date -v-1d)
echo $date
```

## Approfondissement:
Historiquement, la commande `date` est présente depuis les premières versions d'UNIX. En Fish Shell, elle a été améliorée avec l'option `-v` pour ajuster les dates. 

Parmi les alternatives à l'utilisation de la commande `date`, il y a des outils comme `datetime` de Python ou `Date` de JavaScript. Cependant, ces méthodes requièrent souvent d'inclure et d'appeler des librairies supplémentaires, alors que `date` est une commande intégrée à la Fish Shell.

S'agissant des détails d'implémentation, la commande `date` utilise la bibliothèque standard C pour calculer les dates. Elle génère une valeur de temps en secondes, qu'elle ajuste ensuite en fonction des paramètres fournis.

## Voir aussi:
Pour plus d'informations sur le sujet, vous pouvez consulter :
- La page de manuel de `date`: [https://fishshell.com/docs/current/commands.html#date](https://fishshell.com/docs/current/commands.html#date)
- Les caractéristiques de la Fish Shell : [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)  
- Une présentation générale du calcul des dates en programmation: [https://en.wikipedia.org/wiki/Date_(command)](https://en.wikipedia.org/wiki/Date_(command))