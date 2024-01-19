---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'obtention de la longueur d'une chaîne est simplement une opération où l'on compte le nombre de caractères dans une chaîne donnée. Les programmeurs le font pour diverses raisons, comme les contrôles de validation de données ou pour formater correctement l'affichage des données.

## Comment faire:

Dans Fish Shell, vous pouvez trouver la longueur d'une chaîne en utilisant la fonction `string length`. Voici un exemple simple:

```fish
> set ma_chaine "Bonjour, je suis un programmeur"
> string length $ma_chaine 

# Output: 33
```
Dans cet exemple, nous avons défini une variable `ma_chaine` avec notre texte. Puis, nous avons utilisé la fonction `string length` pour trouver la longueur.

## Plongée profonde

L'introduction de la fonction `string` dans Fish Shell a considérablement simplifié les tâches liées aux chaînes de caractères. Avant cela, les programmeurs devaient souvent recourir à des solutions alternatives, moins directes. 

Cependant, il existe d'autres moyens d'obtenir la longueur d'une chaîne. Par exemple, vous pourriez utiliser la commande `wc`, comme ceci: 

```fish
> echo -n $ma_chaine | wc -m

# Output: 33
```

Mais, la fonction `string length` reste le choix le plus simple et le plus rapide.

## Voir aussi 

Pour en savoir plus sur la programmation avec Fish Shell, consultez ces ressources:

- Documentation officielle de Fish Shell : https://fishshell.com/docs/current/index.html
- Guide d'introduction à Fish Shell : https://fishshell.com/docs/current/tutorial.html
- Documentation de la fonction 'string': https://fishshell.com/docs/current/cmds/string.html