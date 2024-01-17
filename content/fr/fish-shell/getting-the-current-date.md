---
title:                "Obtenir la date actuelle"
html_title:           "Fish Shell: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

#Qu'est-ce que la fonction `date` de Fish Shell

## Qu'est-ce que c'est & pourquoi le font les programmeurs?

La fonction `date` de Fish Shell permet aux programmeurs de récupérer la date et l'heure actuelles dans leur script en utilisant une seule commande. Cela évite d'avoir à écrire manuellement la date à chaque fois que cela est nécessaire et assure également que la date utilisée est toujours la plus précise et à jour.

## Comment faire:

Voici comment vous pouvez utiliser la fonction `date` dans votre code Fish Shell:

```
date +%Y-%m-%d 
```

Cela vous donnera la date actuelle au format année-mois-jour, avec le résultat ressemblant à ceci: `2021-05-14`

Vous pouvez également ajouter d'autres options, comme l'heure ou le fuseau horaire, en utilisant le symbole `%` suivi de la lettre correspondante. Par exemple:

```
date +%H:%M:%S 
```

Cela vous donnera l'heure actuelle au format heures-minutes-secondes: `13:25:30`

Pour plus d'options et de détails, vous pouvez consulter la documentation de Fish Shell sur la fonction `date`.

## Plongée en profondeur:

La fonction `date` est un utilitaire bien établi dans les systèmes d'exploitation Unix, qui a été introduit pour la première fois dans les années 1970. En plus de Fish Shell, il peut également être utilisé dans d'autres shells, comme Bash et Zsh. Cependant, Fish Shell offre un paramètre supplémentaire, `%f`, qui permet de formater la date en utilisant les couleurs du shell.

Une alternative à la fonction `date` est la commande `cal`, qui affiche le calendrier du mois en cours, ainsi que la date actuelle. Cependant, contrairement à `date`, `cal` ne permet pas de modifier le format de la date.

## Voir aussi:

Pour en savoir plus sur la fonction `date` de Fish Shell, consultez la documentation officielle: https://fishshell.com/docs/current/cmds/date.html

Pour en savoir plus sur les alternatives à la fonction `date`, vous pouvez consulter cet article sur le forum Unix & Linux: https://unix.stackexchange.com/questions/77541/date-alternative-in-shell-scripts