---
title:    "Fish Shell: Capitaliser une chaîne de caractères"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de la fonction de capitalisation de chaîne est un moyen pratique d'améliorer la lisibilité des données dans Fish Shell. Elle permet de convertir automatiquement les lettres d'une chaîne en majuscules, ce qui peut être utile dans de nombreuses situations de programmation.

## Comment faire

Pour capitaliser une chaîne en Fish Shell, il suffit d'utiliser la syntaxe suivante :

```Fish Shell
set my_string "Bonjour le monde"
echo $my_string | tr '[:lower:]' '[:upper:]'
```

Cela utilisera la commande `tr` pour convertir toutes les lettres de la chaîne en majuscules. Vous pouvez également utiliser la commande `printf` pour obtenir le même résultat :

```Fish Shell
set my_string "Bonjour le monde"
printf "%s" $my_string | tr '[:lower:]' '[:upper:]'
```

Dans les deux cas, la sortie sera "BONJOUR LE MONDE". Il est également possible d'utiliser la fonction `capitalize` qui fait partie du paquet Fish "string" :

```Fish Shell
set my_string "Bonjour le monde"
string capitalize $my_string
```

La sortie sera également "BONJOUR LE MONDE". Toutefois, il convient de noter que la fonction `capitalize` ne prend pas en compte les caractères spéciaux et ne convertira que les lettres en majuscules.

## Plongée en profondeur

La capitalisation de chaîne n'est pas limitée aux seules lettres de l'alphabet, elle peut également être utilisée pour les caractères spéciaux et les chiffres. Par exemple :

```Fish Shell
set my_string "Bonjour le monde !"
echo $my_string | tr '[:lower:]' '[:upper:]'
```

La sortie sera "BONJOUR LE MONDE !". Vous pouvez également capitaliser uniquement le premier caractère d'une chaîne en utilisant la fonction `read` :

```Fish Shell
set my_string "Salut tout le monde"
set -l first_char (string index -i $my_string 1)
string capitalize $first_char; string sub $my_string 2 -1
```

La sortie sera "Salut tout le monde".

## Voir aussi

- [La documentation officielle de Fish Shell](https://fishshell.com/docs/2.7/)
- [La liste complète des fonctions de chaîne de Fish Shell](https://fishshell.com/docs/current/cmds/string.html)
- [Un guide détaillé sur l'utilisation de Fish Shell](https://linoxide.com/linux-command/run-linux-command-shell-script-fish-shell/)