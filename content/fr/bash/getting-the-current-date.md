---
title:                "Bash: Obtention de la date actuelle"
simple_title:         "Obtention de la date actuelle"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il est utile de connaître la date actuelle dans votre script Bash ? Eh bien, la réponse est simple : cela peut vous aider à automatiser certaines tâches en fonction du jour de la semaine, du mois ou de l'année.

## Comment faire

Obtenir la date actuelle dans Bash peut sembler complexe, mais c'est en réalité assez simple. Voici quelques exemples de code pour vous aider à démarrer :

```Bash
# Obtenez la date actuelle au format "AAAA-MM-JJ"
date +"%Y-%m-%d"
```

```Bash
# Obtenez le jour de la semaine (en toutes lettres)
date +"%A"
```

```Bash
# Obtenez le mois (en toutes lettres)
date +"%B"
```

```Bash
# Obtenez l'année (au format court)
date +"%y"
```

Si vous exécutez ces commandes dans votre terminal, vous obtiendrez la date actuelle correspondante en sortie.

## Plongez plus profondément

Maintenant que vous savez comment obtenir la date actuelle dans votre script Bash, voici quelques informations plus avancées pour vous aider à mieux comprendre :

- La commande « date » utilise la variable d'environnement « TZ » pour déterminer le fuseau horaire. Vous pouvez le modifier si nécessaire.
- La commande « date » utilise également la variable d'environnement « LANG » pour déterminer le format de la date. Si vous souhaitez utiliser un format différent, vous pouvez le spécifier en utilisant la commande « date + <format> ».

## Voir aussi

Pour plus d'informations sur l'utilisation de la commande « date » dans Bash, consultez les liens ci-dessous :

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Guide de manipulation des dates dans Bash](https://www.geeksforgeeks.org/date-command-in-linux-with-examples/)
- [Tutoriels pour débutants sur Bash](https://www.codecademy.com/learn/learn-bash)