---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Bash: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?
La capitalisation d'une chaîne de caractères en informatique consiste à mettre la première lettre de chaque mot en majuscule. Les programmeurs le font pour rendre les données plus lisibles et pour respecter les conventions de codage.

## Comment faire:
Voici un exemple de code en Bash pour capitaliser une chaîne de caractères donnée:
```Bash
string="bonjour à tous"
capitalizedString="${string^}"
echo $capitalizedString
```
Résultat: "Bonjour à tous"

Pour capitaliser chaque mot dans une chaîne de caractères, vous pouvez utiliser la commande suivante:
```Bash
string="bonjour à tous"
capitalizedString="${string^^}"
echo $capitalizedString
```
Résultat: "Bonjour À Tous"

## Plongée en profondeur:
La capitalisation de chaîne de caractères est souvent utilisée en programmation pour améliorer la lisibilité des données et pour respecter les conventions de codage. Cette pratique peut être retracée à l'utilisation de la ponctuation dans le langage courant pour améliorer la compréhension des phrases.

Une alternative à la capitalisation de chaîne de caractères est l'utilisation de fonctions telles que ```tr``` ou ```awk```, qui permettent de modifier directement la casse des caractères dans une chaîne.

La commande «${variable^}» en Bash utilise en fait la fonction système ```toupper()``` pour capitaliser la première lettre de la variable. La version en majuscules complètes utilise la fonction ```toupper()``` avec l'option «-t» pour capitaliser chaque caractère de la chaîne.

## Voir aussi:
- Documentation officielle de la commande «string manipulation» en Bash: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Tutoriel sur l'utilisation de la commande ```tr``` pour capitaliser une chaîne de caractères en Bash: https://www.geeksforgeeks.org/tr-command-in-linux-with-examples/