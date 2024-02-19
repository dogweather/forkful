---
aliases:
- /fr/bash/printing-debug-output/
date: 2024-01-20 17:52:04.685758-07:00
description: "Imprimer un d\xE9bogage, c'est afficher des infos dans la console pour\
  \ suivre ce que fait le code. Les d\xE9veloppeurs le font pour rep\xE9rer les bugs\
  \ et\u2026"
lastmod: 2024-02-18 23:09:09.022189
model: gpt-4-1106-preview
summary: "Imprimer un d\xE9bogage, c'est afficher des infos dans la console pour suivre\
  \ ce que fait le code. Les d\xE9veloppeurs le font pour rep\xE9rer les bugs et\u2026"
title: "Affichage des sorties de d\xE9bogage"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi?)
Imprimer un débogage, c'est afficher des infos dans la console pour suivre ce que fait le code. Les développeurs le font pour repérer les bugs et comprendre le flot du programme.

## How to: (Comment faire:)
```Bash
# Affichage simple
echo "Ici, quelque chose a lieu."

# Affichage avec une variable
numero=10
echo "Le numéro est: $numero"

# Utilisation de printf pour un meilleur formatage
printf "Valeur avec deux décimales: %.2f\n" 123.456
```
Sortie :
```
Ici, quelque chose a lieu.
Le numéro est: 10
Valeur avec deux décimales: 123.46
```

## Deep Dive (Plongée en profondeur)
Dans les premiers jours de l'informatique, l'impression de débogage était surtout dirigée vers des imprimantes. Maintenant, on affiche dans une console ou un fichier journal. Il existe d'autres outils comme `sed` et `awk` pour manipuler le texte, mais `echo` et `printf` restent populaires pour leur simplicité. En Bash, `printf` offre plus de contrôle sur le formatage que `echo`.

## See Also (Voir aussi)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
- [Stack Overflow Bash Tag](https://stackoverflow.com/questions/tagged/bash)
