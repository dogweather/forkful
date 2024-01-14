---
title:    "Go: Lecture d'un fichier texte"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Pourquoi lire un fichier texte en Go

Il existe de nombreuses raisons pour lesquelles vous pourriez vouloir lire un fichier texte en utilisant Go. Cela peut-être dans le cadre d'un projet de développement, pour automatiser des tâches ou simplement pour mieux comprendre le fonctionnement de la lecture de fichiers en Go.

# Comment faire

Voici un exemple de code en Go pour lire un fichier texte :

```Go
package main

import (
  "fmt"
  "io/ioutil"
)

func main() {
  // Lecture du fichier texte
  data, err := ioutil.ReadFile("texte.txt")
  if err != nil {
    fmt.Println("Erreur lors de la lecture du fichier :", err)
    return
  }

  // Affichage du contenu
  fmt.Println(string(data))
}
```

Voici le contenu du fichier texte "texte.txt" :

```
Bonjour tout le monde,
Bienvenue sur mon blog de programmation en Go !
```

Et voici l'output de ce programme :

```
Bonjour tout le monde,
Bienvenue sur mon blog de programmation en Go !
```

# Zoom sur la lecture de fichiers texte en Go

La lecture de fichiers en Go se fait principalement avec les fonctions de la librairie standard "io/ioutil". Cette librairie fournit des fonctions pour lire des fichiers et des dossiers. La fonction "ReadFile()" est utilisée pour lire le contenu d'un fichier donné sous forme de tableau d'octets. Ce tableau est ensuite converti en chaîne de caractères pour afficher le contenu du fichier.

# Voir aussi

Voici quelques liens utiles pour en savoir plus sur la lecture de fichiers en Go :

- [Documentation officielle sur la librairie ioutil](https://golang.org/pkg/io/ioutil/)
- [Tutoriel pour lire un fichier CSV en Go](https://medium.com/@matryer/golang-working-with-csv-files-f0ec9eaff559)
- [Article sur la manipulation de fichiers en Go](https://www.calhoun.io/creating-random-files-in-go/)