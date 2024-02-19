---
aliases:
- /fr/go/reading-command-line-arguments/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:14.614052-07:00
description: "Lire les arguments de ligne de commande en Go implique d'extraire les\
  \ arguments fournis \xE0 un programme lors de son invocation depuis le terminal\
  \ ou\u2026"
lastmod: 2024-02-18 23:09:08.254199
model: gpt-4-0125-preview
summary: "Lire les arguments de ligne de commande en Go implique d'extraire les arguments\
  \ fournis \xE0 un programme lors de son invocation depuis le terminal ou\u2026"
title: Lecture des arguments de ligne de commande
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Lire les arguments de ligne de commande en Go implique d'extraire les arguments fournis à un programme lors de son invocation depuis le terminal ou l'invite de commande. Les programmeurs font cela pour personnaliser l'exécution du programme sans modifier le code, rendant les applications plus flexibles et pilotées par l'utilisateur.

## Comment faire :

Go offre un accès direct aux arguments de ligne de commande à travers le package `os`, spécifiquement en utilisant `os.Args`, un tableau de chaînes de caractères. Voici un exemple simple pour commencer :

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args fournit un accès aux arguments de ligne de commande bruts
    fmt.Println("Arguments de ligne de commande :", os.Args)

    if len(os.Args) > 1 {
        // Boucler à travers les arguments, en sautant le premier (nom du programme)
        for i, arg := range os.Args[1:] {
            fmt.Printf("Argument %d : %s\n", i+1, arg)
        }
    } else {
        fmt.Println("Aucun argument de ligne de commande fourni.")
    }
}
```

Un exemple de sortie lors de l'exécution avec `go run yourprogram.go arg1 arg2` pourrait ressembler à :

```
Arguments de ligne de commande : [/tmp/go-build123456789/b001/exe/yourprogram arg1 arg2]
Argument 1 : arg1
Argument 2 : arg2
```

Cela imprime tous les arguments y compris le nom du programme (souvent à l'index 0), puis itère sur chaque argument fourni, les imprimant. Pour un parsing d'arguments plus contrôlé, vous pourriez considérer le package `flag` pour l'analyse des options de ligne de commande.

## Plongée en profondeur

Historiquement, l'accès aux arguments de ligne de commande est une pratique aussi ancienne que la programmation en C, où `argc` et `argv[]` servent un but similaire. En Go, `os.Args` le rend simple mais délibérément rudimentaire. Pour des scénarios plus complexes, tels que la gestion des drapeaux ou des options, Go offre le package `flag` qui fournit des capacités d'analyse robustes. Cela pourrait être vu comme une alternative "meilleure" lorsque votre application nécessite plus que de simples arguments positionnels.

Contrairement à certaines langues de script qui offrent une analyse intégrée des arguments de ligne de commande en tableaux associatifs ou en objets, l'approche de Go exige que les programmeurs gèrent manuellement l'analyse en utilisant `os.Args` pour les besoins de base ou qu'ils tirent parti du package `flag` pour des scénarios plus avancés. Cette conception reflète la philosophie de Go de maintenir le langage de base simple tout en fournissant de puissantes bibliothèques standard pour les tâches courantes. Bien que cela puisse introduire une légère courbe d'apprentissage pour ceux qui sont habitués à l'analyse intégrée, cela offre une plus grande flexibilité et encourage une compréhension plus profonde de la gestion des arguments de ligne de commande.
