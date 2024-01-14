---
title:    "Fish Shell: Calculer une date dans le futur ou le passé."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou dans le passé peut être extrêmement utile pour programmer des tâches ou pour vérifier des échéances. Avec Fish Shell, cette tâche peut être réalisée rapidement et facilement grâce à sa syntaxe simple et intuitive.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en utilisant Fish Shell, suivez ces étapes :

1. Ouvrez votre terminal et lancez la commande `fish` pour entrer dans l'interpréteur de commandes Fish Shell.
2. Utilisez la commande `date` suivie du symbole `+` et d'une valeur numérique, par exemple `+2`, pour indiquer le nombre de jours que vous voulez avancer dans le temps. Vous pouvez également utiliser le symbole `-` pour reculer dans le temps.
3. Ajoutez une unité de temps après la valeur numérique pour préciser combien de jours, mois, années, etc. vous voulez avancer ou reculer. Par exemple, `+2d` pour avancer de deux jours ou `-1m` pour reculer d'un mois.

```
Fish Shell  ~> date +2d
lun. août 30 16:05:05 CEST 2021
Fish Shell ~> date -1m
sam. juil. 31 16:05:05 CEST 2021
```

## Plongée en profondeur

Il est possible d'utiliser plusieurs unités de temps à la fois pour calculer une date dans le futur ou dans le passé. Voici quelques exemples :

- `+2y3mo`pour avancer de 2 années et 3 mois
- `-1w5d` pour reculer d'1 semaine et 5 jours
- `+1y2h30m` pour avancer d'1 année, 2 heures et 30 minutes

Il est également possible d'utiliser des formules mathématiques dans les valeurs numériques pour effectuer des calculs plus complexes. Par exemple, `+1w+3d` pour ajouter 1 semaine et 3 jours ou `+2*3mo` pour ajouter 6 mois.

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/)
- [Tutoriel pour débutants sur Fish Shell](https://www.linux.com/topic/desktop/getting-started-fish-shell/)
- [Exemples d'utilisation avancée de Fish Shell](https://hackernoon.com/a-year-this-fish-instead-of-bash-40-lessons-learned-e9e90eb83a24) (en anglais)