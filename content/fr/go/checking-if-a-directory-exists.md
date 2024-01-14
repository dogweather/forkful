---
title:                "Go: Vérification de l'existence d'un répertoire"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous programmez en Go, il est courant de rencontrer des situations où vous devez vérifier si un répertoire existe avant de poursuivre l'exécution du code. Cela peut sembler une tâche simple, mais savoir comment le faire correctement peut vous faire gagner du temps et vous éviter des erreurs dans votre code.

# Comment faire

Il existe plusieurs façons de vérifier si un répertoire existe en utilisant Go. La méthode la plus simple consiste à utiliser la fonction `os.Stat()` qui renvoie des informations sur un fichier ou un répertoire donné. Si le répertoire n'existe pas, la fonction renverra une erreur, sinon, elle renverra les informations sur le répertoire.

Voici un exemple de code utilisant `os.Stat()` pour vérifier si un répertoire existe :

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Vérifie si le répertoire "documents" existe
    if _, err := os.Stat("documents"); os.IsNotExist(err) {
        fmt.Println("Le répertoire n'existe pas")
    } else {
        fmt.Println("Le répertoire existe")
    }
}
```

Si vous souhaitez simplement vérifier si un répertoire existe sans avoir à récupérer des informations sur celui-ci, vous pouvez utiliser la fonction `os.IsExist()` qui renvoie un booléen indiquant si le fichier ou le répertoire existe ou non.

```Go
// Vérifie si le répertoire "documents" existe
if os.IsExist("documents") {
    fmt.Println("Le répertoire existe")
} else {
    fmt.Println("Le répertoire n'existe pas")
}
```

# Plongez plus en profondeur

En creusant un peu plus, vous découvrirez qu'il existe d'autres façons de vérifier si un répertoire existe en utilisant des packages tels que `filepath` ou `io/ioutil`. Mais l'essentiel est de savoir comment utiliser correctement `os.Stat()` et `os.IsExist()` pour éviter des erreurs dans votre code.

# Voir aussi

- [Documentation officielle sur la fonction os.Stat()](https://golang.org/pkg/os/#Stat)
- [Documentation officielle sur la fonction os.IsExist()](https://golang.org/pkg/os/#IsExist)
- [Article de blog sur la gestion des erreurs en Go](https://blog.golang.org/error-handling-and-go)