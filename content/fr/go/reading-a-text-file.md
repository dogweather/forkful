---
title:                "Go: Lecture d'un fichier texte"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi

L'utilisation de fichiers textes est une pratique courante en programmation. Cela permet de stocker des données de manière structurée et facilement accessible. Dans cet article, nous allons explorer comment lire un fichier texte en utilisant le langage de programmation Go.

# Comment Faire

Pour lire un fichier texte en Go, nous allons utiliser la fonction `ioutil.ReadFile()` qui prend en paramètre le chemin du fichier à lire et renvoie un tableau de bytes contenant le contenu du fichier. Nous pouvons ensuite convertir ce tableau en une chaîne de caractères en utilisant la fonction `string()`.

Pour illustrer cela, prenons l'exemple d'un fichier texte `texte.txt` contenant le texte suivant :

```Go
Je suis un fichier texte.
```

Nous pouvons alors lire le contenu du fichier et l'afficher sur la console en utilisant le code suivant :

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	filePath := "texte.txt"
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		fmt.Printf("Erreur lors de la lecture du fichier : %v", err)
	}
	fmt.Println(string(content))
}
```

La sortie de ce programme sera la suivante :

```Go
Je suis un fichier texte.
```

# Plongée Profonde

Il est important de noter que la méthode `ioutil.ReadFile()` lit le fichier entier en une seule fois, ce qui peut être problématique pour les fichiers volumineux. Dans ce cas, il est préférable d'utiliser la fonction `os.Open()` pour ouvrir le fichier et la méthode `bufio.Scanner` pour lire le fichier ligne par ligne.

De plus, si notre fichier texte contient des données structurées, telles que des données structurées en JSON ou en CSV, il peut être utile d'utiliser des packages spécifiques pour ce type de données, tels que `encoding/json` ou `encoding/csv`.

# Voir Aussi

- [Documentation officielle de la fonction ioutil.ReadFile()](https://golang.org/pkg/io/ioutil/#ReadFile)
- [Documentation officielle de la fonction os.Open()](https://golang.org/pkg/os/#Open)
- [Documentation officielle de la méthode bufio.Scanner](https://golang.org/pkg/bufio/#Scanner)
- [Package encoding/json](https://golang.org/pkg/encoding/json/)
- [Package encoding/csv](https://golang.org/pkg/encoding/csv/)