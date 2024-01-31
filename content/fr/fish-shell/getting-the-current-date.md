---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:14:06.719855-07:00
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Obtenir la date actuelle, c'est savoir quel jour on est. Les programmeurs l'utilisent pour des logs, des timestamps, ou pour gérer des événements basés sur la date.

## How to:
Voici comment afficher la date et l'heure actuelles dans Fish :

```Fish Shell
set current_date (date)
echo $current_date
```

Et si on veut juste la date sans l'heure :

```Fish Shell
set current_date (date +%Y-%m-%d)
echo $current_date
```

Résultat pour la deuxième commande, par exemple :

```
2023-04-01
```

## Deep Dive
Historiquement, Fish, short pour "friendly interactive shell", s'est distingué par sa simplicité et ses fonctionnalités avancées. En comparant avec d'autres shells, comme Bash, Fish propose un scripting plus clair et des fonctions intégrées qui simplifient de nombreuses tâches. 

Pour la date, `date` est une commande Unix standard, aussi accessible dans Fish. Les alternatives incluent l'utilisation de commandes intégrées Fish ou de scripts externes, mais `date` reste un choix fiable et universel.

Au niveau de l'implémentation, `date` se sert du système d'exploitation pour obtenir les informations temporelles. Avec le paramètre `+%Y-%m-%d`, on formate cette sortie pour obtenir une chaîne de caractères représentant l'année, le mois et le jour.

## See Also
- Pour savoir plus sur la commande `date` : [man7.org/linux/man-pages/man1/date.1.html](https://man7.org/linux/man-pages/man1/date.1.html)
- Comparaison entre Fish et autres shells : [github.com/fish-shell/fish-shell/wiki/Design-Philosophy](https://github.com/fish-shell/fish-shell/wiki/Design-Philosophy)
