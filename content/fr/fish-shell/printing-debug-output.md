---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'affichage des sorties de débogage est une technique qu'utilisent les programmeurs pour suivre l'exécution d'un programme et identifier les erreurs. C'est comme une conversation en temps réel avec votre code.

## Comment faire:
Avec Fish Shell, utiliser `echo` pour imprimer des sorties de débogage. 

```fish
set nom "Pascal"
echo "Bonjour, $nom"
```

La sortie sera : 
```fish
Bonjour, Pascal
```

Pour afficher plus détails, vous pouvez aussi utiliser `printf`.

```fish
set age 25
printf "Salut, je suis %s et j'ai %d ans\n" $nom $age
```

La sortie sera :
```fish
Salut, je suis Pascal et j'ai 25 ans
```

## Plongée en profondeur
Historiquement, l'affichage des sorties de débogage est une technique qui date des premiers temps de la programmation. C'est un outil essentiel pour comprendre comment un programme fonctionne. Toutefois, il existe des alternatives, comme l'utilisation d'un débogueur step-by-step, mais l'utilisation de `echo` et `printf` reste une méthode rapide et simple. Vous devez cependant savoir que chaque utilisation de `echo` ou `printf` créée une sous-commande, ce qui peut ralentir l'exécution de votre script sur des machines plus faibles ou surcharger dans un script complexe.

## Voir aussi
Pour une exploration plus approfondie, ces ressources sont utiles:

- La documentation officielle de la commande `echo`: http://fishshell.com/docs/current/commands.html#echo
- Un tutoriel sur `printf` en Fish: https://fishshell.com/docs/current/tutorial.html#tut_printf
- Un guide détaillé sur le débogage en Fish Shell: https://github.com/fish-shell/fish-shell/issues/1362