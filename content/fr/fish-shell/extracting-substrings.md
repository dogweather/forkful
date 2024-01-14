---
title:    "Fish Shell: Extraction de sous-chaînes"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous avez peut-être déjà rencontré des situations où vous aviez besoin d'extraire une partie d'une chaîne de caractères spécifique. Cela peut sembler être une tâche simple, mais cela peut devenir plus complexe lorsque vous travaillez avec de grandes quantités de données. C'est là que la fonction d'extraction de sous-chaînes entre en jeu. En utilisant Fish Shell, vous pouvez facilement extraire des sous-chaînes d'une chaîne de caractères pour répondre à vos besoins spécifiques.

## Comment faire
Pour extraire une sous-chaîne dans Fish Shell, vous pouvez utiliser la commande `string sub` suivie de votre chaîne de caractères et des indices de début et de fin pour spécifier les parties de la chaîne que vous souhaitez extraire. Par exemple:
```
Fish Shell:
string sub "Bonjour tout le monde" 8 14
```
Le résultat de cette commande sera "tout le".

Vous pouvez également utiliser des variables pour spécifier les indices de début et de fin au lieu de les mettre directement dans la commande. Vous pouvez le faire en utilisant la syntaxe `$start` et `$end` à la place des indices numériques. Voici un exemple:
```
Fish Shell:
start=8
end=14
string sub "Bonjour tout le monde" $start $end
```
Le résultat sera toujours "tout le".

Vous pouvez également extraire une sous-chaîne en utilisant des motifs de correspondance au lieu d'indices numériques. Par exemple, si vous voulez extraire tout ce qui se trouve avant le premier espace dans une chaîne de caractères, vous pouvez utiliser la commande suivante:
```
Fish Shell:
string match "Bonjour tout le monde" "* "
```
Le résultat sera "Bonjour".

## Plongée en profondeur
En plus des exemples ci-dessus, il existe différentes options pour personnaliser votre extraction de sous-chaîne en utilisant Fish Shell. Par exemple, vous pouvez utiliser des options telles que `--all`, `--inclusive`, `--count` ou encore `--lines` pour obtenir des résultats spécifiques. Vous pouvez également utiliser des expressions régulières pour extraire des sous-chaînes en utilisant la commande `string replace`.

## Voir aussi
Pour en savoir plus sur les différentes possibilités d'extraction de sous-chaînes dans Fish Shell, consultez les ressources suivantes:
- Le site officiel de Fish Shell: https://fishshell.com/
- La documentation de l'extraction de sous-chaînes dans Fish Shell: https://fishshell.com/docs/current/cmds/string.html#String-sub
- Un tutoriel détaillé sur l'utilisation de la commande `string sub`: https://ostechnix.com/how-to-extract-particular-parts-of-a-file-name-in-linux/