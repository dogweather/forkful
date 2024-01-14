---
title:                "Gleam: Affichage des résultats de débogage"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de messages de débogage (debug output) est un outil utile pour les programmeurs lorsqu'ils écrivent du code. Cela permet de comprendre comment le programme s'exécute et de localiser plus facilement les erreurs.

## Comment faire

Pour imprimer un message de débogage en Gleam, vous pouvez utiliser la fonction ```io.debug()``` suivie du message que vous souhaitez afficher. Par exemple :

```
Gleam io.debug("Valeur de la variable x : {}", x)
```

Cela affichera le contenu de la variable ```x``` entre les accolades dans la console pendant l'exécution du programme. Vous pouvez également utiliser plusieurs paramètres entre accolades pour afficher plusieurs variables ou valeurs différentes.

## Plongée en profondeur

L'impression de messages de débogage peut également être utilisée pour vérifier l'état d'une variable à différents endroits dans le code. Vous pouvez insérer des messages de débogage à des endroits stratégiques pour suivre la valeur d'une variable tout au long de l'exécution du programme. Cela peut être particulièrement utile lors du débogage de boucles ou de fonctions récursives.

De plus, il est possible de personnaliser le niveau de débogage en utilisant une variable d'environnement. Par exemple, en utilisant la variable ```GLEAM_DEBUG``` avec la valeur ```all```, tous les messages de débogage seront affichés, même ceux qui ont été écrits dans des modules tiers.

## Voir aussi

- [Documentation Gleam sur les messages de débogage](https://gleam.run/book/tour/debugging.html)
- [Article sur l'utilisation des messages de débogage en pratique](https://blog.gleam.run/debugging-your-code-with-gleam/)
- [Video sur le débogage en Gleam](https://www.youtube.com/watch?v=EUjI2jpeHRY)