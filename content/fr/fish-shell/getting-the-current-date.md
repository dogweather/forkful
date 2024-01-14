---
title:    "Fish Shell: Obtenir la date actuelle"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Qu'il s'agisse de mettre à jour des fichiers ou de planifier des tâches, connaître la date actuelle est une base essentielle pour tout programmeur. Dans cet article, nous allons vous expliquer comment utiliser Fish Shell pour obtenir la date actuelle dans tous vos scripts.

## Comment faire

```Fish Shell
set current_date (date +%d/%m/%Y)
echo $current_date
```

Voici un exemple simple de code Fish Shell pour obtenir la date actuelle dans le format jour/mois/année. Le résultat sera affiché dans la console lorsque vous exécutez le script. Mais il existe de nombreuses façons d'obtenir la date dans différents formats en utilisant des commandes différentes.

```Fish Shell
set current_time (date +%H:%M:%S)
echo $current_time
```

Vous pouvez également utiliser la commande `date +%H:%M:%S` pour obtenir l'heure actuelle au format heure:minute:seconde. Vous pouvez combiner différentes commandes pour obtenir des formats de date et d'heure personnalisés selon vos besoins.

```Fish Shell
set full_date (date +%A,%d %B %Y)
echo $full_date
```

Si vous avez besoin d'afficher la date complète avec le jour de la semaine, le jour du mois et le mois, utilisez la commande `date +%A,%d %B %Y`. Vous pouvez modifier l'ordre ou le format en fonction de vos préférences.

Vous pouvez également utiliser la commande `man date` pour obtenir la liste complète des options et des formats de date disponibles dans Fish Shell.

## Plongée profonde

Il est important de noter que la commande `date` dépend du fuseau horaire de votre système. Si vous souhaitez obtenir la date actuelle dans un fuseau horaire spécifique, vous pouvez utiliser la commande `TZ` suivie du fuseau horaire souhaité, puis la commande `date`, comme ceci :

```Fish Shell
set timezone (TZ=Europe/Paris date +%d/%m/%Y)
echo $timezone
```

Vous pouvez également utiliser des variables pour stocker différentes dates et les utiliser ultérieurement dans votre script :

```Fish Shell
set tomorrow (date --date="1 day" +%d/%m/%Y)
set yesterday (date --date="-1 day" +%d/%m/%Y)
```

Enfin, n'oubliez pas de mettre à jour votre date et heure système si vous rencontrez des problèmes en utilisant la commande `set` car cela peut interférer avec les valeurs stockées de manière temporaire.

## Voir aussi

- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/cmds/date.html)
- [Guide de référence rapide pour les commandes du Shell Fish](https://fishshell.com/docs/current/cmds.html)
- [Comment afficher l'heure et la date actuelles dans Fish Shell](https://www.linuxsecrets.com/fish-shell/afficher-lheure-date-actuelles-shell-fish/)
- [Maîtrisez les commandes de date et d'heure dans Fish Shell](https://www.ostechnix.com/master-date-time-commands-fish-shell/)