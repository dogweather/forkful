---
title:                "Fish Shell: Obtenir la date actuelle"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi
Obtenir la date et l'heure actuelles est une tâche courante dans la programmation. Cela peut être utile pour afficher l'information la plus récente ou pour enregistrer des données avec un horodatage précis. Dans cet article, nous allons explorer comment obtenir la date actuelle en utilisant Fish Shell.

# Comment Faire
Pour obtenir la date actuelle en Fish Shell, nous pouvons utiliser la commande "date" suivi du format désiré. Par exemple, si nous souhaitons afficher la date au format "jj/mm/aaaa", nous pouvons utiliser la commande suivante dans notre terminal:

```
date +%d/%m/%Y
```

Cela affichera la date actuelle au format "jour/mois/année". Nous pouvons également ajouter d'autres informations telles que l'heure ou le fuseau horaire en utilisant des options supplémentaires avec la commande "date". Par exemple, pour afficher la date et l'heure complètes au format ISO 8601, nous pouvons utiliser la commande suivante:

```
date +%Y-%m-%dT%H:%M:%S%z
```

Cela affichera l'heure et la date au format "année-mois-jourTheure:minute:seconde+hhmm", avec le fuseau horaire inclus. Il existe de nombreuses autres options que vous pouvez explorer pour personnaliser votre sortie de date et d'heure.

# Plongée Profonde
La commande "date" dans Fish Shell est une interface vers la fonction de système "strftime" qui est utilisée pour formater les informations de date et d'heure. Vous pouvez trouver la liste complète des spécificateurs de format disponibles dans la documentation officielle de Fish Shell. En utilisant ces spécificateurs, vous pouvez personnaliser votre sortie en affichant la date et l'heure de différentes manières.

# Voir Aussi
- Documentation officielle de Fish Shell sur la commande "date": https://fishshell.com/docs/current/commands.html#date
- Documentation officielle de Fish Shell sur la fonction système "strftime": https://fishshell.com/docs/current/index.html#strftime
- Tutoriel sur les bases de Fish Shell: https://hackernoon.com/a-gentle-introduction-to-fish-shell-5tj3