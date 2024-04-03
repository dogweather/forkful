---
date: 2024-01-20 17:46:49.071632-07:00
description: "(Mesurer la longueur d'une cha\xEEne : Quoi et Pourquoi?) Calculer la\
  \ longueur d'une cha\xEEne de caract\xE8res, c'est compter le nombre de ses \xE9\
  l\xE9ments. C'est\u2026"
lastmod: '2024-03-13T22:44:57.979891-06:00'
model: gpt-4-1106-preview
summary: "(Mesurer la longueur d'une cha\xEEne : Quoi et Pourquoi."
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
weight: 7
---

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
