---
title:                "Convertir une chaîne en minuscules"
html_title:           "Fish Shell: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Pourquoi
Pourquoi convertir une chaîne de caractères en minuscules ? Eh bien, c'est généralement utile lorsqu'on souhaite uniformiser le format d'une chaîne pour faciliter les comparaisons ou l'affichage.

# Comment faire
La conversion d'une chaîne en minuscules est assez simple en utilisant la Shell Fish. Tout d'abord, il faut utiliser la fonction `string tolower` suivie de la chaîne à convertir entre parenthèses. Voici un exemple pour mieux comprendre :

```Fish Shell
string tolower "Voici Une Phrase En Majuscules"
```

Cela renverra la même phrase en minuscules : "voici une phrase en majuscules". Vous pouvez également stocker le résultat dans une variable en utilisant l'opérateur d'affectation `=`.

# Plongeons plus profondément
En réalité, la fonction `string tolower` utilise la variable interne `$LANG` pour déterminer le jeu de caractères à utiliser lors de la conversion. Si vous souhaitez utiliser un autre jeu de caractères, vous pouvez le spécifier en utilisant l'option `-c` suivie de la valeur du jeu de caractères. Voici un exemple :

```Fish Shell
string tolower -c UTF-8 "übeRSTRommen"
```

Cela renverra "überstrommen", en utilisant le jeu de caractères UTF-8 pour la conversion.

# Voir aussi
- [Documentation officielle Fish Shell](https://fishshell.com/docs/current/)
- [Recherche sur les variables internes Fish Shell](https://fishshell.com/docs/current/index.html#internals)
- [Chaînes de caractères en Shell Fish](https://fishshell.com/docs/current/cmds/string.html)