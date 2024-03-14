---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:13.782899-07:00
description: "\xC9crire sur l'erreur standard (stderr) en Go implique de diriger les\
  \ messages d'erreur ou les diagnostics qui ne sont pas destin\xE9s au flux de sortie\u2026"
lastmod: '2024-03-13T22:44:57.152687-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire sur l'erreur standard (stderr) en Go implique de diriger les messages\
  \ d'erreur ou les diagnostics qui ne sont pas destin\xE9s au flux de sortie\u2026"
title: "\xC9criture sur l'erreur standard"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire sur l'erreur standard (stderr) en Go implique de diriger les messages d'erreur ou les diagnostics qui ne sont pas destinés au flux de sortie principal. Les programmeurs utilisent cela pour séparer la sortie régulière des informations d'erreur, rendant le débogage et l'analyse des logs plus simples.

## Comment faire :

En Go, le package `os` fournit la valeur `Stderr`, représentant le fichier d'erreur standard. Vous pouvez l'utiliser avec les fonctions `fmt.Fprint`, `fmt.Fprintf`, ou `fmt.Fprintln` pour écrire sur stderr. Voici un exemple simple :

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Écrire une simple chaîne sur stderr
    _, err := fmt.Fprintln(os.Stderr, "Ceci est un message d'erreur !")
    if err != nil {
        panic(err)
    }

    // Message d'erreur formaté avec Fprintf
    errCount := 4
    _, err = fmt.Fprintf(os.Stderr, "Processus terminé avec %d erreurs.\n", errCount)
    if err != nil {
        panic(err)
    }
}
```

Exemple de sortie (sur stderr) :
```
Ceci est un message d'erreur !
Processus terminé avec 4 erreurs.
```

Rappelez-vous, ces messages n'apparaîtront pas dans la sortie régulière (stdout) mais dans le flux d'erreur, qui peut être redirigé séparément dans la plupart des systèmes d'exploitation.

## Approfondissement

Le concept d'erreur standard est profondément ancré dans la philosophie Unix, qui distingue clairement entre sortie normale et messages d'erreurs pour un traitement et une gestion des données plus efficaces. En Go, cette convention est embrassée à travers le package `os`, qui fournit un accès direct aux descripteurs de fichier stdin, stdout, et stderr.

Alors qu'écrire directement sur `os.Stderr` convient à de nombreuses applications, Go fournit également des packages de journalisation plus sophistiqués comme `log`, qui offrent des fonctionnalités supplémentaires telles que l'horodatage et des configurations de sortie plus flexibles (par exemple, l'écriture dans des fichiers). Utiliser le package `log`, en particulier pour des applications plus importantes ou là où des fonctionnalités de journalisation plus complètes sont nécessaires, peut être une meilleure alternative. Il est également à noter que l'approche de Go en matière de gestion des erreurs, qui encourage le retour des erreurs à partir des fonctions, complète la pratique d'écrire des messages d'erreur sur stderr, permettant un contrôle plus granulaire de la gestion et du rapport des erreurs.

En essence, alors qu'écrire sur stderr est une tâche fondamentale dans de nombreux langages de programmation, la bibliothèque standard de Go et ses principes de conception offrent à la fois des voies simples et avancées pour gérer la sortie des erreurs, s'alignant sur les pratiques industrielles plus larges tout en répondant également à l'éthos de conception spécifique de Go.
