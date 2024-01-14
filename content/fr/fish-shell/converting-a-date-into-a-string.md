---
title:    "Fish Shell: Transformer une date en chaîne de caractères"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un développeur à la recherche de moyens d'afficher des dates dans votre script Fish Shell, vous êtes au bon endroit ! Dans cet article, nous allons vous montrer comment convertir facilement une date en une chaîne de caractères.

## Comment faire
La conversion d'une date en une chaîne de caractères peut sembler un peu intimidante, mais ne vous inquiétez pas, c'est assez simple avec Fish Shell. Voici un exemple de code qui utilise la commande `date` pour obtenir la date et la convertit en une chaîne de caractères en utilisant la commande `string` :

```Fish Shell
set myDate (date -u +"%Y-%m-%d")
set myString (string join '-' $myDate)
```
Lorsque vous exécutez ce code dans votre terminal, la sortie sera quelque chose comme ceci :

```
2021-07-23
```
Vous pouvez également personnaliser le format de la date en utilisant les options disponibles dans la commande `date`. Voici un exemple qui ajoute l'heure et les minutes à la date :

```Fish Shell
set myDate (date -u +"%Y-%m-%d %H:%M")
set myString (string join '-' $myDate)
```
Et voici la sortie correspondante :

```
2021-07-23 12:30
```

## Plongée en profondeur
Maintenant que vous savez comment convertir une date en une chaîne de caractères, vous pouvez vous plonger dans les différentes options disponibles dans la commande `date`. Cela peut vous aider à personnaliser la sortie de votre chaîne de caractères en fonction de vos besoins spécifiques.

Par exemple, vous pouvez ajouter le jour de la semaine à votre chaîne de caractères en utilisant `%a` pour les abréviations ou `%A` pour le nom complet du jour :

```Fish Shell
set myDate (date -u +"%Y-%m-%d %a")
set myString (string join '-' $myDate)
```
La sortie sera alors quelque chose comme :

```
2021-07-23 Fri
```

Vous pouvez également utiliser `%b` pour les abréviations du mois ou `%B` pour le nom complet du mois. Pour plus d'options, vous pouvez consulter la documentation de la commande `date`.

## Voir aussi
Pour plus d'informations sur la conversion de dates en chaînes de caractères dans Fish Shell, consultez ces liens utiles :

- La documentation officielle de la commande `date` (en anglais) : https://fishshell.com/docs/current/commands.html#date
- Un tutoriel sur les variables dans Fish Shell (en français) : https://doc.ubuntu-fr.org/tutoriel/comment_definir_une_variable_dans_un_script_shell
- Une liste complète des options pour formater les dates avec la commande `date` (en anglais) : https://www.computerhope.com/unix/date.htm