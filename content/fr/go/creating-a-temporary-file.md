---
title:                "Go: Création d'un fichier temporaire"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en Go ?

La création de fichiers temporaires est une pratique courante en programmation, notamment en Go. Ces fichiers servent à stocker temporairement des données qui seront utilisées dans une tâche spécifique et qui ne nécessitent pas d'être enregistrées de manière permanente. Ils peuvent également être utiles pour gérer des ressources limitées telles que la mémoire ou le disque dur. Dans cet article, nous allons voir comment créer un fichier temporaire en utilisant le langage de programmation Go.

## Comment créer un fichier temporaire en Go ?

La création d'un fichier temporaire en Go est assez simple. Tout d'abord, nous devons importer le package "io/ioutil" qui contient les fonctions nécessaires pour créer et manipuler des fichiers temporaires.

```Go
import "io/ioutil"
```

Ensuite, nous pouvons utiliser la fonction "TempFile" pour créer un fichier temporaire dans un répertoire spécifique avec un préfixe donné. Cette fonction renvoie un pointeur vers le fichier et une erreur, que nous devons gérer.

```Go
f, err := ioutil.TempFile("/chemin/vers/le/repertoire", "prefixe_")
```

Nous pouvons maintenant écrire dans ce fichier temporaire en utilisant la méthode "WriteString". Par exemple, nous pouvons écrire un message dans le fichier et le sauvegarder en utilisant la méthode "Close".

```Go
f.WriteString("Contenu du fichier temporaire")
f.Close()
```

Nous pouvons également récupérer le nom du fichier temporaire en utilisant la méthode "Name" et le supprimer en utilisant la méthode "Remove".

```Go
nom_fichier := f.Name()
f.Remove()
```

Voici un exemple complet de création et de manipulation d'un fichier temporaire en Go.

```Go
import (
    "io/ioutil"
    "fmt"
)

func main() {
    f, err := ioutil.TempFile("/chemin/vers/le/repertoire", "prefixe_")
    if err != nil {
        fmt.Println(err)
    }
    defer f.Close()

    f.WriteString("Contenu du fichier temporaire")
    fmt.Println("Le fichier temporaire est créé :", f.Name())

    nom_fichier := f.Name()
    err = f.Remove()
    if err != nil {
        fmt.Println(err)
    }
    fmt.Println("Le fichier temporaire est supprimé :", nom_fichier)
}
```

## Plongée en profondeur

La fonction "TempFile" utilise une bibliothèque système pour générer un nom de fichier unique et crée effectivement le fichier en utilisant "os.OpenFile". Le préfixe que nous lui avons fourni est ajouté au nom généré. Ensuite, il est possible d'utiliser les méthodes "fd.Name", "fd.Write" et "fd.Remove" sur le pointeur renvoyé. La méthode "Close" s'assure que les ressources utilisées par le fichier temporaire sont libérées une fois qu'on n'en a plus besoin.

## Voir aussi

- Package "io/ioutil" documentation : https://golang.org/pkg/io/ioutil/
- "os" package documentation : https://pkg.go.dev/os
- Plus d'informations sur les fichiers temporaires en Go : https://blog.alexellis.io/golang-read-write-create-files/