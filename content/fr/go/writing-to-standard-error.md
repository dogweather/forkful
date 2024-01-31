---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
simple_title:         "Écrire dans l'erreur standard"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
En Go, écrire dans l'erreur standard (`stderr`) permet de séparer les messages d'erreur des autres sorties. On le fait pour diagnostiquer les problèmes sans interférer avec la sortie (`stdout`) utilisée pour les données.

## How to:
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Fprintln(os.Stderr, "Ceci est un message d'erreur.")
}
```
Sortie dans `stderr` :
```
Ceci est un message d'erreur.
```

## Deep Dive
Traditionnellement, Unix distingue la sortie standard (`stdout`) et l'erreur standard (`stderr`) pour permettre des redirections flexibles. En Go, `os.Stderr` est un `*os.File`, similaire à `os.Stdout`. On utilise souvent `log` ou `fmt` avec `os.Stderr` pour l'implémentation. Des alternatives avec différentes bibliothèques comme `logrus` offrent plus d'options de formatage et de gestion des erreurs.

## See Also
- Documentation Go sur `os`: https://golang.org/pkg/os/
- Tutoriel sur la gestion d'erreur en Go: https://blog.golang.org/error-handling-and-go
- Projet `logrus`, une alternative puissante au paquet `log` de Go: https://github.com/sirupsen/logrus
