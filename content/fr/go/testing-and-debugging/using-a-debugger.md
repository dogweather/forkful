---
title:                "Utiliser un débogueur"
aliases: - /fr/go/using-a-debugger.md
date:                  2024-02-03T18:10:03.394298-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utiliser un débogueur"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/using-a-debugger.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Utiliser un débogueur en programmation Go implique l'utilisation d'outils ou de fonctionnalités pour inspecter et modifier l'état d'un programme en cours d'exécution afin de comprendre son comportement ou diagnostiquer des problèmes. Les programmeurs font cela pour trouver et corriger efficacement des bugs, optimiser les performances et assurer la correction de leur code.

## Comment faire :

Go fournit une facilité intégrée pour le débogage appelée `delve`. C'est un outil de débogage complet qui vous permet d'exécuter des programmes Go pas à pas, d'inspecter les variables du programme et d'évaluer des expressions.

Pour commencer, vous devez d'abord installer `delve`. Vous pouvez le faire en exécutant :

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Maintenant, déboguons un simple programme Go. Considérez un programme `main.go` :

```go
package main

import "fmt"

func main() {
    message := "Débogage en Go"
    fmt.Println(message)
}
```

Pour commencer le débogage de ce programme, ouvrez un terminal dans le répertoire du projet et exécutez :

```shell
dlv debug
```

Cette commande compile le programme avec les optimisations désactivées (pour améliorer l'expérience de débogage), le démarre et attache un débogueur à celui-ci.

Une fois `delve` en cours d'exécution, vous êtes dans l'interpréteur interactif du débogueur. Voici quelques commandes de base :

- `break main.main` définit un point d'arrêt dans la fonction `main`.
- `continue` reprend l'exécution du programme jusqu'à ce qu'un point d'arrêt soit atteint.
- `print message` affichera la valeur de la variable `message`.
- `next` avance l'exécution du programme à la ligne suivante.
- `quit` quitte le débogueur.

La sortie lorsque le point d'arrêt est atteint et que la variable est affichée pourrait ressembler à ceci :

```shell
Breakpoint 1 at 0x49ekcf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (frappe goroutine(1):1 total:1) (PC : 0x49ekcf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Débogage en Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Débogage en Go"
```

En utilisant ces commandes, vous pouvez progresser pas à pas dans votre programme, inspectant l'état au fur et à mesure pour comprendre comment il se comporte et identifier tout problème.

## Approfondissement

Le choix de `delve` comme outil de débogage de prédilection pour Go, en préférence aux outils traditionnels comme GDB (GNU Debugger), est principalement dû à la nature du modèle d'exécution et du runtime de Go. GDB n'a pas été conçu initialement avec le runtime de Go à l'esprit, rendant `delve` un choix plus adapté pour les développeurs Go. `Delve` est spécifiquement conçu pour Go, offrant une expérience de débogage plus intuitive pour les routines Go, les canaux et autres constructions spécifiques à Go.

De plus, `delve` prend en charge un large éventail de fonctionnalités allant au-delà de celles offertes par GDB de base lors du travail avec des programmes Go. Celles-ci incluent, mais ne se limitent pas à : l’attachement à des processus en cours pour le débogage ; les points d'arrêt conditionnels ; et l’évaluation d’expressions complexes pouvant impliquer les primitives de concurrence de Go.

Bien que `delve` soit le débogueur de choix pour de nombreux développeurs Go, il convient de noter que la chaîne d'outils Go inclut également des formes de support de débogage plus légères, telles que l’outil intégré `pprof` pour le profilage et l’outil `trace` pour la visualisation de la concurrence. Ces outils peuvent parfois fournir une avenue plus rapide ou de plus haut niveau pour diagnostiquer des problèmes de performance de programme ou des bugs de concurrence, qui pourraient être complémentaires ou même préférables selon le contexte de débogage.
