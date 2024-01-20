---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

L'impression de sortie de débogage est un moyen pour les programmeurs d'afficher des messages d'erreur, des variables et d'autres données pour vérifier le fonctionnement du programme. C'est essentiel pour identifier et résoudre les problèmes rapidement.

## Comment faire:

Gleam vous permet d'imprimer le débogage avec la fonction `io.debug`. Prenez cet exemple:

```Gleam
import gleam/io

fn main() {
  let a = 100
  io.debug(a) // Affiche "DEBUG: 100\n" dans la console
}
```
Lors de l'exécution de ce programme, `100` sera affiché dans la sortie de débogage.

## Plongée profonde

Historiquement, l'impression de débogage est une pratique ancienne qui remonte aux premiers jours de la programmation. En Gleam, c'est une fonctionnalité simple mais essentielle qui incorpore le meilleur des langages de programmation fonctionnels et impératifs.

Une autre alternative possible pour débugger en Gleam est d'utiliser une interface débogueur avec un EDI comme Visual Studio Code, qui offre un environnement pour surveiller activement les variables et l'état du programme.

Quant à l'implémentation, `io.debug` fonctionne en affichant des messages sur `stderr`; ce qui signifie que le débogage n'affecte pas la sortie standard du programme, garantissant ainsi que seul le développeur verra les messages de débogage.

## Voir aussi

Pour en savoir plus sur le débogage en Gleam, consultez les ressources suivantes :

- [Gleam Language Guide](https://gleam.run/book/tour/)
- [Visual Studio Code - Debugger Extension](https://code.visualstudio.com/docs/editor/debugging)