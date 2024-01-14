---
title:                "Java: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi
Imprimer du débogage (ou debugging en anglais) est un outil précieux pour les programmeurs. Cela leur permet de vérifier le fonctionnement de leur code et d'identifier les problèmes éventuels.

## Comment faire
Pour imprimer du débogage en Java, utilisez la méthode `System.out.println()`. Elle affichera la valeur que vous souhaitez vérifier dans la console. Par exemple :

```Java
int x = 5;  
System.out.println(x);  
```

Cela affichera `5` dans la console.

## Plongée en profondeur
Il existe plusieurs façons d'optimiser l'impression du débogage en Java. Par exemple, vous pouvez utiliser des formats de chaînes pour afficher des informations plus complexes, ou utiliser un Logger pour enregistrer les informations de débogage dans un fichier. Vous pouvez également utiliser des indicateurs de débogage pour n'imprimer que du débogage dans certains scénarios.

## Voir aussi
- [Documentation officielle de Java sur l'impression du débogage](https://docs.oracle.com/javase/tutorial/essential/io/formatting.html)
- [Tutoriel vidéo sur l'impression du débogage en Java](https://www.youtube.com/watch?v=QcRMkXp9PvU)
- [Comparaison entre System.out.println() et Logger](https://javaconceptoftheday.com/system-out-println-vs-logger/)