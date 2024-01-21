---
title:                "Trouver la longueur d'une chaîne de caractères"
date:                  2024-01-20T17:46:49.071632-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
(Mesurer la longueur d'une chaîne : Quoi et Pourquoi?)
Calculer la longueur d'une chaîne de caractères, c'est compter le nombre de ses éléments. C'est utile pour valider des données ou manipuler du texte.

## How to:
(Comment faire :)
Obtenir la longueur d'une chaîne de caractères en Bash :

```Bash
#!/bin/bash
ma_chaine="Bonjour à tous"
longueur=${#ma_chaine}
echo "La longueur est: $longueur"
```

Sortie :

```
La longueur est: 14
```

## Deep Dive
(Plongée en profondeur)
Historiquement, le fait de mesurer la longueur d’une chaîne de caractères intègre les bases de la programmation. C’est essentiel pour gérer le stockage en mémoire et pour prévenir les erreurs de débordement de tampon. En Bash, le préfixe `$#` avant une variable renvoie directement sa longueur, méthode pratique et directe sans appel à une fonction externe.

L'alternative, moins directe, impliquerait l'utilisation de `expr` ou `wc` :

```Bash
longueur_expr=$(expr length "$ma_chaine")
longueur_wc=$(echo -n "$ma_chaine" | wc -m)
```

L’implémentation se base sur l'interprétation de chaîne de caractères. Bash considère une chaîne comme une séquence de caractères et le `$#` permet d'accéder à cette information de manière native.

## See Also
(Voir également)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)