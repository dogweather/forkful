---
title:                "Créer un fichier temporaire"
html_title:           "Go: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi créer un fichier temporaire en Go ?

Vous vous demandez peut-être pourquoi il serait utile de créer un fichier temporaire lorsque vous programmez en Go. Eh bien, cela peut être très pratique lorsque vous avez besoin de stocker temporairement des données ou de manipuler des fichiers sans risquer de les écraser ou de les modifier de manière permanente.

## Comment faire ?

Voici un exemple de code en Go pour créer un fichier temporaire :

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    // Création d'un fichier temporaire
    f, err := ioutil.TempFile("", "example")

    if err != nil {
        // Gestion des erreurs
        fmt.Println(err)
        return
    }

    // Ecriture dans le fichier temporaire
    _, err = f.Write([]byte("Ceci est un fichier temporaire"))

    if err != nil {
        // Gestion des erreurs
        fmt.Println(err)
    }

    // Fermeture du fichier
    f.Close()

    // Suppression du fichier temporaire à la fin du programme
    defer os.Remove(f.Name())

    // Affichage du nom du fichier temporaire créé
    fmt.Println("Fichier temporaire créé avec succès :", f.Name())
}
```

Après l'exécution de ce code, vous devriez voir le message suivant : "Fichier temporaire créé avec succès : **nom_fichier_temporaire**". Ce fichier sera automatiquement supprimé à la fin du programme grâce à l'utilisation de la fonction `defer`.

## Deep Dive

En utilisant la fonction `ioutil.TempFile`, vous pouvez spécifier le répertoire dans lequel vous souhaitez créer le fichier temporaire et un préfixe pour le nom du fichier. Si vous ne spécifiez pas de répertoire, le fichier sera créé dans le répertoire système par défaut pour les fichiers temporaires.

La fonction `ioutil.TempFile` renvoie un objet `File` qui peut être utilisé pour écrire ou lire des données dans le fichier temporaire.

Il est important de noter que tout comme pour les fichiers normaux, il est nécessaire de fermer et de supprimer le fichier temporaire une fois que vous avez fini de l'utiliser.

# Voir aussi

Pour en savoir plus sur la création et la manipulation de fichiers en Go, vous pouvez consulter les liens suivants :

- [Documentation officielle de Go sur les fichiers](https://golang.org/pkg/os/#File)
- [Tutoriel sur Go par le site officiel](https://golang.org/doc/tutorial/create-files)
- [Documentation officielle de Go sur les fonctions ioutil](https://golang.org/pkg/io/ioutil/)