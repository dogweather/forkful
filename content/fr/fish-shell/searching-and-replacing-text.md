---
title:                "Rechercher et remplacer du texte"
html_title:           "Fish Shell: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un utilisateur régulier de Fish Shell, vous savez probablement à quel point il est pratique de travailler avec du texte. Mais parfois, il peut être fastidieux de modifier manuellement du texte dans plusieurs fichiers. Heureusement, Fish Shell a une fonctionnalité de recherche et de remplacement qui peut vous faire gagner du temps et de l'énergie.

## Comment faire
La syntaxe de base pour rechercher et remplacer du texte dans Fish Shell est la suivante :

```Fish Shell
set variable (echo $variable | sed 's/pattern/replacement/')
```

Dans cet exemple, nous utilisons la commande `sed` pour rechercher et remplacer le texte contenu dans la variable `$variable`. Vous pouvez remplacer `echo $variable` par n'importe quelle autre commande qui renvoie du texte, comme `cat file.txt`.

Jetons un coup d'œil à un exemple concret. Supposons que nous ayons un fichier texte avec plusieurs occurrences du mot "chat" et que nous voulions le remplacer par "chien". Nous pouvons utiliser cette commande :

```Fish Shell
set file_contents (cat file.txt | sed 's/chat/chien/g')
```

Cette commande recherche toutes les occurrences de "chat" dans le fichier et les remplace par "chien". Le flag `g` à la fin de la commande indique que la recherche doit être effectuée de manière globale, c'est-à-dire pour toutes les occurrences du mot "chat". Sans ce flag, la recherche ne s'effectuerait que pour la première occurrence.

Il est également possible d'utiliser des expressions régulières pour une recherche et un remplacement plus précis. Par exemple, si nous souhaitons remplacer toutes les occurrences de nombres dans un fichier par "123", nous pouvons utiliser cette commande :

```Fish Shell
set file_contents (cat file.txt | sed 's/[0-9]+/123/g')
```

Ici, l'expression régulière `[0-9]+` correspond à n'importe quel nombre présent dans le fichier.

## Zoom en profondeur
Si vous souhaitez en savoir plus sur les expressions régulières et leur utilisation dans Fish Shell, vous pouvez consulter la page de manuel de `sed` et la documentation de la fonction Fish Shell `string`. Ces ressources vous donneront une compréhension plus approfondie de la syntaxe à utiliser et des différentes options disponibles pour la recherche et le remplacement de texte dans Fish Shell.

## Voir aussi
- [La page de manuel de `sed`](http://www.gnu.org/software/sed/manual/sed.html)
- [La documentation de Fish Shell pour la fonction `string`](https://fishshell.com/docs/current/cmds/string.html)