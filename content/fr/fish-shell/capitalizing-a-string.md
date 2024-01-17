---
title:                "Mettre en majuscule une chaîne de caractères"
html_title:           "Fish Shell: Mettre en majuscule une chaîne de caractères"
simple_title:         "Mettre en majuscule une chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi les programmeurs le font?

La capitalisation d'une chaîne de caractères est simplement le fait de mettre en majuscules la première lettre de chaque mot. Les programmeurs le font souvent pour améliorer la lisibilité du code.

# Comment faire:

```
fish_shell
set my_string "hello world"
string capitalize $my_string
```

Output: "Hello World"

```
fish_shell
set my_string "this is an example"
string capitalize $my_string
```

Output: "This Is An Example"

# Focus sur:

## Contexte historique:
La capitalisation des chaînes de caractères remonte aux premiers langages de programmation comme le Fortran ou le COBOL. Aujourd'hui, sa popularité continue en raison de la volonté des développeurs de maintenir un code propre et facile à comprendre.

## Alternatives:
Il existe d'autres méthodes pour capitaliser une chaîne de caractères, telles que l'utilisation de fonctions de bibliothèque ou la manipulation des chaînes de caractères en boucle. Cependant, l'utilisation de la fonction intégrée "string capitalize" dans Fish Shell est rapide, simple et efficace.

## Détails de mise en œuvre:
Dans Fish Shell, la fonction "string capitalize" utilise la bibliothèque de chaînes de caractères Go pour effectuer la capitalisation. Si vous préférez une autre méthode, vous pouvez également utiliser des expressions régulières ou des fonctions de traitement de chaînes de caractères à cet effet.

# À voir également:

Plus d'informations sur la fonction "string capitalize" de Fish Shell peuvent être trouvées dans la documentation officielle : https://fishshell.com/docs/current/cmds/string.html#string-capitalize