---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:58.915537-07:00
description: "Comment faire : En Go, le paquet `os` fournit des fonctionnalit\xE9\
  s pour interagir avec le syst\xE8me d'exploitation, y compris v\xE9rifier l'existence\
  \ d'un\u2026"
lastmod: '2024-03-13T22:44:57.150419-06:00'
model: gpt-4-0125-preview
summary: "En Go, le paquet `os` fournit des fonctionnalit\xE9s pour interagir avec\
  \ le syst\xE8me d'exploitation, y compris v\xE9rifier l'existence d'un r\xE9pertoire."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

## Comment faire :
En Go, le paquet `os` fournit des fonctionnalités pour interagir avec le système d'exploitation, y compris vérifier l'existence d'un répertoire. Voici comment vous pouvez le faire :

```go
package main

import (
    "fmt"
    "os"
)

// isDirExists vérifie si un répertoire existe
func isDirExists(path string) bool {
    info, err := os.Stat(path)
    if os.IsNotExist(err) {
        return false
    }
    return info.IsDir()
}

func main() {
    dirPath := "/tmp/exampleDir"

    if isDirExists(dirPath) {
        fmt.Printf("Le répertoire %s existe.\n", dirPath)
    } else {
        fmt.Printf("Le répertoire %s n'existe pas.\n", dirPath)
    }
}
```
Exemple de sortie :

```
Le répertoire /tmp/exampleDir existe.
```
ou

```
Le répertoire /tmp/exampleDir n'existe pas.
```

Selon que `/tmp/exampleDir` existe ou non.

## Approfondissement
La fonction `os.Stat` retourne une interface `FileInfo` et une erreur. Si l'erreur est de type `os.ErrNotExist`, cela signifie que le répertoire n'existe pas. S'il n'y a pas d'erreur, nous vérifions alors si le chemin référence effectivement un répertoire grâce à la méthode `IsDir()` de l'interface `FileInfo`.

Cette méthode se distingue par sa simplicité et son efficacité, mais il est important de noter que vérifier l'existence d'un répertoire avant d'effectuer des opérations telles que la création ou l'écriture pourrait conduire à des conditions de compétition dans des environnements concurrentiels. Pour de nombreux scénarios, en particulier dans les applications concurrentielles, il pourrait être plus sûr de tenter l'opération (par exemple, la création de fichiers) et de gérer les erreurs par la suite, plutôt que de vérifier au préalable.

Historiquement, cette approche a été courante dans la programmation en raison de sa logique simple. Cependant, l'évolution de l'informatique multi-thread et concurrente nécessite un changement vers une gestion des erreurs plus robuste et d'éviter, autant que possible, les vérifications de préconditions comme celle-ci. Cela ne diminue pas son utilité pour des applications ou des scripts plus simples, mono-thread, où de telles conditions sont moins préoccupantes.
