---
title:                "C: Afficher la sortie de débogage"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur chevronné en C, vous avez probablement déjà rencontré des problèmes avec votre code. Que ce soit une erreur de syntaxe ou un problème de logique, le débogage est un élément essentiel du processus de développement. Dans cet article, nous allons aborder une technique simple et efficace pour vous aider à résoudre ces problèmes : l'impression des sorties de débogage.

## Comment faire

L'impression des sorties de débogage est une technique qui consiste à afficher des informations utiles pendant l'exécution de votre programme. Pour cela, nous allons utiliser la fonction `printf`, qui permet d'afficher des données à l'écran.

Voici un exemple de code utilisant `printf` pour décrire un problème de division :

```C
int a = 10, b = 0;
printf("a/b = %d \n", a/b);
```

Lorsque ce code est exécuté, vous obtiendrez une erreur de division par zéro. Mais grâce à l'impression de l'opération a/b, vous pourrez facilement identifier le problème et le corriger.

## Plongée dans les détails

Maintenant que vous savez comment utiliser `printf` pour déboguer votre code, voici quelques conseils pour une utilisation plus avancée :

- Utilisez plusieurs `printf` à différents endroits de votre code pour suivre et comprendre l'évolution des variables.
- Vous pouvez également utiliser la fonction `fprintf` pour imprimer les sorties de débogage dans un fichier plutôt que sur l'écran.
- N'oubliez pas de supprimer toutes les instructions de débogage avant de passer en production.

## Voir aussi

Pour en savoir plus sur le débogage en C, voici quelques liens utiles :

- [Débogage avec GDB](https://www.gnu.org/software/gdb/)
- [Techniques avancées de débogage en C](https://www.codeproject.com/Articles/12577/Advanced-Debugging-Techniques)
- [Débogage avec Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-basics?view=vs-2019)

Utilisez ces techniques pour améliorer votre processus de développement et résoudre plus efficacement les problèmes dans votre code. N'oubliez pas que l'impression des sorties de débogage est un outil précieux, alors utilisez-le à bon escient !