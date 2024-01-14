---
title:    "Go: Vérifier si un répertoire existe"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

L'objectif principal de tout programmeur est de s'assurer que son code fonctionne de manière fluide et sans problème. Une façon d'y parvenir est de vérifier si un répertoire existe avant de procéder à d'autres actions. Cela peut éviter des erreurs imprévues et contribuer à la stabilité du code.

## Comment faire

En utilisant le langage de programmation Go, il est possible de vérifier si un répertoire existe en utilisant la fonction `os.Stat()`. Cette fonction vérifie si un fichier ou un répertoire existe à partir du chemin d'accès fourni.

Voici un exemple de code qui vérifie si un répertoire existe et imprime un message en conséquence :

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Définir un chemin d'accès au répertoire à vérifier
	dirPath := "./documents"

	// Vérifier si le répertoire existe
	if _, err := os.Stat(dirPath); err != nil {
		fmt.Println("Le répertoire n'existe pas")
	} else {
		fmt.Println("Le répertoire existe")
	}
}
```

Dans cet exemple, nous utilisons la fonction `os.Stat()` et vérifions si une erreur est renvoyée. Si c'est le cas, cela signifie que le répertoire n'existe pas et nous imprimons un message correspondant. Sinon, cela signifie que le répertoire existe et nous imprimons un autre message.

## Plongée en profondeur

Outre la fonction `os.Stat()`, il existe d'autres façons de vérifier si un répertoire existe en utilisant Go. Par exemple, la fonction `os.IsNotExist()` peut être utilisée pour vérifier si une erreur est due à un répertoire n'existant pas. De plus, il est également possible d'utiliser des opérations de fichier telles que `Readdir()` pour obtenir la liste des fichiers et répertoires existants et les comparer avec le chemin d'accès fourni.

## Voir aussi

- Documentation officielle de Go sur la fonction `os.Stat()` : https://golang.org/pkg/os/#Stat
- Tutoriel sur la vérification de l'existence d'un répertoire en Go : https://www.digitalocean.com/community/tutorials/how-to-check-if-a-directory-exists-in-go
- Exemple de code utilisant la fonction `os.IsNotExist()` : https://gobyexample.com/error-handling