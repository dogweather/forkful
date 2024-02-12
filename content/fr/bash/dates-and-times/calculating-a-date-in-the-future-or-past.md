---
title:                "Calcul d'une date future ou passée"
aliases:
- /fr/bash/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:28:37.090090-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcul d'une date future ou passée"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date dans le futur ou le passé c'est juste dire à un ordinateur de trouver une date qui est tant de jours avant ou après une autre date. Les programmeurs font ça pour planifier des événements, renouveler des abonnements, rappeler des deadlines, ou tout scénario où le temps est un facteur clé.

## Comment faire :
```Bash
# Ajouter 10 jours à la date actuelle
date -d "+10 days"

# Soustraire 10 jours à la date actuelle
date -d "-10 days"

# Afficher le résultat
date +%Y-%m-%d -d "2022-03-15 +10 days" # 2022-03-25
date +%Y-%m-%d -d "2022-03-15 -10 days" # 2022-03-05
```

## Exploration plus profonde
Historiquement, les calculs de dates étaient plus compliqués avec les versions antérieures de Bash et d'autres outils de ligne de commande. Avant, on pouvait utiliser `date` avec des syntaxes spécifiques ou se tourner vers des programmes externes comme `ncal` ou `cal`. Maintenant, `date` manipule tout simplement les dates en avant ou en arrière.

Il y a d'autres moyens de calculer les dates, comme des scripts Perl ou Python si Bash ne suffit pas, ou pour des besoins plus complexes.

Côté implémentation, chaque commande `date` génère un nouvel 'epoch timestamp', calcule le décalage, puis formatte le tout en une date lisible. C'est solide car ça gère les années bissextiles et les irrégularités du calendrier.

## Voir aussi
- GNU Coreutils `date`: [lien](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- Advanced Bash-Scripting Guide - Date and Time: [lien](https://tldp.org/LDP/abs/html/datetime.html)
- UNIX `cal`: [lien](https://man7.org/linux/man-pages/man1/cal.1.html)
