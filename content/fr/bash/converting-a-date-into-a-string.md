---
title:    "Bash: Transformer une date en chaîne de caractères"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaine de caractères est une tâche courante en programmation Bash. Cela peut être utile pour afficher des dates dans un format spécifique ou lors de la manipulation de données temporelles.

## Comment faire

Pour convertir une date en chaine de caractères, nous pouvons utiliser la commande `date` suivie de l'option `+%m%d%y` pour spécifier le format souhaité. Par exemple, si nous voulons afficher la date au format `mois/jour/année`, nous pouvons utiliser la commande suivante :

```Bash
date +%m/%d/%Y

# Output: 04/15/2021
```

Nous pouvons également utiliser d'autres options pour personnaliser le format de la date, telles que `%H` pour afficher l'heure au format 24 heures ou `%I` pour l'afficher au format 12 heures.

## Deep Dive

Lorsque nous utilisons la commande `date` pour convertir une date en chaine de caractères, elle utilise un formatage de date selon les paramètres régionaux du système d'exploitation par défaut. Cela signifie que le format de la date peut varier en fonction de la configuration de la machine.

Pour éviter cela, nous pouvons utiliser l'option `-u` pour forcer l'utilisation du fuseau horaire universel (UTC). Cela garantit que la date sera toujours convertie dans un format spécifique, indépendamment des paramètres régionaux du système d'exploitation.

## Voir aussi

- La documentation officielle de la commande `date` : https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Un tutoriel sur la manipulation de dates en Bash : https://www.tecmint.com/working-with-dates-and-time-in-linux-bash-scripting/