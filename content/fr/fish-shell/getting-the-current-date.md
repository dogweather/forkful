---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Fish Shell: Récupérer la date actuelle

## Pourquoi et Pourquoi faire?

Obtenir la date actuelle signifie récupérer le jour actuel selon le calendrier. Les programmeurs font cela pour enregistrer des informations chronologiques, comme le moment exact où une action se produit.

## Comment Faire:

En Fish Shell, le code suivant renvoie la date et l'heure actuelles:

```Fish Shell
date
```

Cela devrait retourner quelque chose ressemblant à cela:

```Fish Shell
Tue May 28 15:52:02 PDT 2021
```

Si vous voulez juste le jour du mois, alors utilisez ce code:

```Fish Shell
date +%D
```

Ce qui donne:

```Fish Shell
05/28/21
```

## Plongée Profonde

Historiquement, dans le Unix original (1969), il n'y avait pas réellement de manière standard pour obtenir la date. `date` est venu plus tard (1983) pour standardiser cette fonctionnalité à travers les systèmes Unix.

Il existe diverses alternatives à la fonction `date`. Vous pourriez par exemple, pour certaines tâches spécifiques, utiliser `strftime` ou `clock_gettime`.

Au niveau de l'implémentation, la commande `date` en Fish fait essentiellement un appel système pour récupérer l'horloge du système, qui est maintenue par le système d'exploitation.

## Voir Aussi

Plus d'informations peuvent être trouvées sur la commande `date` en Fish Shell: 
- La page de manuel de `date`: https://fishshell.com/docs/current/cmds/date.html
- Un guide sur la programmation en Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Une explication plus détaillée de `strftime`: http://man7.org/linux/man-pages/man3/strftime.3.html