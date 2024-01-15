---
title:                "Extraction de sous-chaînes"
html_title:           "Fish Shell: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous utilisez Fish Shell, vous savez probablement à quel point il est agréable de travailler avec ce shell moderne et convivial. Mais saviez-vous que vous pouvez également utiliser des sous-chaînes dans vos commandes Fish Shell ? Cela peut sembler intimidant au premier abord, mais en réalité c'est un moyen pratique de manipuler et d'extraire des parties spécifiques de vos chaînes de texte. Dans cet article, nous allons vous montrer comment utiliser les sous-chaînes dans votre code Fish Shell et vous expliquer pourquoi cela peut être utile.

## Comment faire
Si vous voulez extraire une sous-chaîne d'une chaîne de texte, la première chose à faire est de comprendre comment fonctionnent les indices de sous-chaînes dans Fish Shell. Les indices commencent à 1 et les caractères spéciaux sont également pris en compte. Voici un exemple de code pour extraire une sous-chaîne :

```Fish Shell
set ma_chaine "Bonjour tout le monde !"
set sous_chaine (string sub --start 9 --length 4 $ma_chaine)
echo $sous_chaine
```

Dans cet exemple, nous créons une chaîne "ma_chaine" contenant la phrase "Bonjour tout le monde !" et nous utilisons la commande "string sub" pour extraire une sous-chaîne à partir du 9ème caractère pour une longueur de 4 caractères. Le résultat sera "tout". Vous pouvez également utiliser des indices négatifs pour compter à partir de la fin de la chaîne. Par exemple, un indice de -1 correspond au dernier caractère de la chaîne. Voici un autre exemple pour extraire le dernier mot de notre chaîne d'origine :

```Fish Shell
set dernier_mot (string sub --start -7 --length 7 $ma_chaine)
echo $dernier_mot
```

Le résultat sera "monde !".

## Plongée Plus Profonde
Il est important de noter que les sous-chaînes sont immuables dans Fish Shell, ce qui signifie qu'elles ne peuvent pas être modifiées. Si vous essayez de modifier une sous-chaîne, vous obtiendrez un message d'erreur. De plus, si vous utilisez un indice de sous-chaîne qui dépasse la longueur de la chaîne d'origine, une sous-chaîne vide sera retournée. Enfin, si vous omettez l'argument de longueur, la sous-chaîne sera extraite jusqu'à la fin de la chaîne d'origine.

Maintenant que vous avez compris les bases de l'utilisation des sous-chaînes dans Fish Shell, vous pouvez les utiliser dans vos commandes pour manipuler toutes sortes de chaînes de texte. Que ce soit pour extraire des noms de fichiers, des adresses e-mail, ou tout autre élément spécifique d'une chaîne, les sous-chaînes sont un outil utile à avoir dans votre arsenal de programmation Fish Shell.

## Voir Aussi
Pour en savoir plus sur l'utilisation des sous-chaînes dans Fish Shell, consultez la documentation officielle sur les sous-chaînes : https://fishshell.com/docs/current/cmds/string-sub.html

Et si vous voulez aller encore plus loin dans votre apprentissage de Fish Shell, n'hésitez pas à explorer les différentes commandes et fonctionnalités offertes par ce shell moderne et puissant. Bonne programmation !