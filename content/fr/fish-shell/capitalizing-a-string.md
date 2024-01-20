---
title:                "Mettre une chaîne en majuscules"
html_title:           "Fish Shell: Mettre une chaîne en majuscules"
simple_title:         "Mettre une chaîne en majuscules"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?
Capitaliser une chaîne, c'est convertir le premier caractère d'une chaîne en majuscule. Les programmeurs le font pour améliorer la lisibilité du contenu ou pour respecter certaines normes de codage.

## Comment faire:
Voici comment vous pouvez capitaliser une chaîne dans le shell Fish :

```fish
function capitalize
    set string $argv
    echo (string sub -l 1 $string | tr '[:lower:]' '[:upper:]')(string sub -s 2 $string)
end
```
Ceci est un exemple d'utilisation et de sortie :
```fish
> capitalize "fish shell"
Fish shell
```
Cela prend votre chaîne "fish shell" et la convertit en "Fish shell".

## Plongée profonde
L'idée de capitalisation des chaînes a été introduite depuis l'aube de la programmation. L'objectif principal est l'amélioration de la lisibilité, ce qui est crucial pour le débogage et la compréhension du code par d'autres programmeurs.

Il existe des alternatives pour capitaliser une chaîne dans d'autres langages, par exemple, en utilisant la méthode `.capitalize` en Python ou la méthode `.toUpperCase` en JavaScript.

Sur le plan de l'implémentation, notez que `tr` dans Fish Shell remplace les minuscules par des majuscules. `string sub -l 1` extrait le premier caractère de la chaîne, alors que `string sub -s 2` extrait tous les caractères après le premier, laissant le premier inchangé.

## Voir aussi
Pour plus d'informations, consultez ces liens :
- [Documentation sur Fish Shell](https://fishshell.com/docs/3.1/commands.html)
- [Guide de programmation Fish Shell](https://github.com/fish-shell/fish-shell)
- [Manipulation des chaînes en Fish Shell](https://fishshell.com/docs/current/index.html#string)