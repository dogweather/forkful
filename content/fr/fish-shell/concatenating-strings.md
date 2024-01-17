---
title:                "Concaténation de chaînes de caractères"
html_title:           "Fish Shell: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

La concaténation de chaînes de caractères est simplement le fait de joindre plusieurs chaînes ensemble pour en créer une plus longue. Les programmeurs font cela souvent pour simplifier le code et pour créer des chaînes de caractères plus spécifiques pour leurs programmes.

## Comment faire:

Voici quelques exemples de code en utilisant le Fish Shell pour concaténer des chaînes de caractères :

```
Fish Shell -c echo "Bonjour " "mon ami!"
```

Output: Bonjour mon ami!

```
Fish Shell "* " (seq 1 5)
```

Output: 1 2 3 4 5

## Plongeon en profondeur:

La concaténation de chaînes existe depuis longtemps dans la programmation, et est également utilisée dans d'autres langages tels que Python et Java. Cependant, certains programmeurs préfèrent utiliser des alternatives telles que les tableaux de chaînes de caractères ou les fonctions pour concaténer des chaînes. Le Shell Fish utilise la commande `echo` pour concaténer des chaînes, mais il existe également d'autres commandes telles que `printf` qui peuvent être utilisées.

## Voir aussi:

Pour plus d'informations sur la concaténation de chaînes de caractères dans le Fish Shell, veuillez consulter la documentation officielle : https://fishshell.com/docs/current/cmds/echo.html