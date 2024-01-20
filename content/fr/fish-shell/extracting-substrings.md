---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?
Extraire des sous-chaînes dans la programmation signifie recueillir une partie spécifique d'une chaîne de caractères. C'est utilisé couramment pour analyser ou manipuler l'information dans des scénarios spécifiques.

## Comment faire :
Voici comment vous pouvez extraire une sous-chaîne en utilisant Fish Shell.

Pour extraire une sous-chaîne à partir d'un indice spécifique, utilisez la commande `string sub`.

```Fish Shell
set string "Bonjour tout le monde"
string sub -s 9 -l 4 $string
```
Output:
```Fish Shell
tout
```
Dans l'exemple ci-dessus, `-s 9` spécifie l'indice de départ de la sous-chaîne (le 9ème caractère) et `-l 4` spécifie la longueur de la sous-chaîne (4 caractères).

## Plongée profonde :
L'extraction des sous-chaînes est en fait une technique utilisée depuis les premiers jours de la programmation. C'est un outil essentiel qui s'avère utile dans de nombreux contextes, que ce soit pour l'analyse syntaxique, le traitement de texte ou l'interaction avec les APIs.

Il existe de nombreuses alternatives à `string sub` dans Fish, comme l'utilisation de `cut` ou `awk`, mais `string sub` est généralement plus simple et plus facile à comprendre.

En ce qui concerne l'implémentation, Fish utilise une approche basée sur les indices pour extraire des sous-chaînes. L'indice de départ est inclusif et l'indice de fin est exclusif.

## Voir aussi :
Pour plus d'informations sur l'extraction de sous-chaînes et d'autres fonctionnalités de Fish, consultez les liens suivants.

- Documentation officielle de Fish Shell : https://fishshell.com/docs/current/index.html
- Formation sur le Fish Shell: https://github.com/jorgebucaran/fisher
- Guide de programmation Fish Shell: https://github.com/jbucaran/awesome-fish