---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Go: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi? 
Contrôler si un répertoire existe consiste à vérifier qu'un dossier spécifique est présent sur le système de fichiers. Les programmeurs le font pour éviter les erreurs lors de l'accès, de la lecture ou de l'écriture des données dans ce dossier.

## Comment faire :
Voici comment vous pouvez faire cela en Go:

``` Go
package main

import (
    "fmt"
    "os"
)

func main() {
    dirPath := "/chemin/vers/le/dossier"
    _, err := os.Stat(dirPath)

    if os.IsNotExist(err) {
        fmt.Printf("Le répertoire %v n'existe pas\n", dirPath)
    } else {
        fmt.Printf("Le répertoire %v existe\n", dirPath)
    }
}
```

Dans cet exemple, si le répertoire existe, il affiche "Le répertoire /chemin/vers/le/dossier existe". Sinon, il affiche "Le répertoire /chemin/vers/le/dossier n'existe pas".

## Plongée en profondeur 
La fonction globale "os.Stat()" a été introduite dans Go depuis la sortie initiale du langage en 2007. Elle renvoie une structure "FileInfo" et une erreur. Si l'erreur est de type "IsNotExist", alors le fichier ou le répertoire n'est pas présent. 

Il y a d'autres façons de faire cela. La bibliothèque "ioutil" de Go a aussi une fonction "ReadDir" qui peut être utilisée pour lister tous les fichiers dans un répertoire et vérifier si un répertoire spécifique y existe. C'est plus utile lorsque vous voulez faire plus qu'une simple vérification d'existence.

La bibliothèque "os" emploie des appels système pour vérifier l'existence des répertoires, ce qui signifie que sa performance dépend du système d'exploitation sous-jacent.

## Voir aussi 
Pour plus d'informations sur la bibliothèque "os" et comment l'utiliser pour interagir avec le système de fichiers, vous pouvez consulter ce lien: https://pkg.go.dev/os 

Pour une discussion détaillée sur le traitement des erreurs en Go (comme "os.IsNotExist(err)"), regardez ceci: https://blog.golang.org/error-handling-and-go