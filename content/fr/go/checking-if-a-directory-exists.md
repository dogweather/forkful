---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:56:22.521231-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? - Quoi et Pourquoi ?
Vérifier qu'un répertoire existe évite des erreurs lorsque votre code s'attend à travailler avec. C'est une étape préventive avant de lire, écrire ou modifier des fichiers.

## How to: - Comment faire :
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Define the path to check
	dirPath := "/path/to/dir"

	// Use os.Stat function and check for errors
	if _, err := os.Stat(dirPath); os.IsNotExist(err) {
		fmt.Println("Le répertoire n'existe pas.")
	} else {
		fmt.Println("Le répertoire existe.")
	}
}
```

Résultat possible:
```
Le répertoire existe.
```
ou
```
Le répertoire n'existe pas.
```

## Deep Dive - Plongée en profondeur
Historiquement, le contrôle de l'existence d'un répertoire est primordial pour éviter les erreurs de 'file not found' qui peuvent faire échouer un programme. Sous Unix et Unix-like systèmes, comme Linux, le concept de fichier est profondément ancré; tout est fichier, y compris les répertoires.

En Go, `os.Stat` renvoie des infos de fichiers ou un code d'erreur si le fichier n'existe pas. Une alternative est `os.IsNotExist(err)`, une fonction utile pour localiser l'erreur spécifique 'fichier ou répertoire non trouvé'. Le package `io/fs` apporte aussi des fonctionnalités pour interagir avec les systèmes de fichiers, en reflétant la philosophie de Go pour créer des systèmes robustes et fiables.

L'implémentation repose sur l'interrogation du système de fichiers de l'OS sous-jacent. C'est efficace, mais soumis aux permissions d'accès et à la sincérité du système de fichiers.

## See Also - Voir aussi
- Documentation officielle de Go pour `os.Stat`: https://golang.org/pkg/os/#Stat
- Un guide sur la gestion des erreurs en Go: https://blog.golang.org/error-handling-and-go
- Paquetage `io/fs`: https://golang.org/pkg/io/fs/
