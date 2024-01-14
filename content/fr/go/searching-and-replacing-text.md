---
title:    "Go: Rechercher et remplacer du texte"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Pourquoi

La recherche et le remplacement de texte sont des tâches courantes dans la programmation, en particulier lors de la manipulation de grandes quantités de données. En utilisant le langage de programmation Go, vous pouvez facilement automatiser ce processus et économiser du temps et des efforts lors de la manipulation de vos données.

# Comment faire

Pour réaliser une recherche et un remplacement de texte en utilisant Go, vous pouvez utiliser les fonctions de la bibliothèque strings. Par exemple, si vous voulez remplacer toutes les occurrences d'un mot dans une chaîne de caractères, vous pouvez utiliser la fonction `ReplaceAll` de la manière suivante :

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Bonjour tout le monde, comment ça va ?"

    // Recherche et remplace "bonjour" par "salut"
    newStr := strings.ReplaceAll(str, "bonjour", "salut")

    fmt.Println(newStr)
}
```

Lorsque vous exécutez ce code, vous obtiendrez comme sortie : "Salut tout le monde, comment ça va ?". Vous pouvez également utiliser d'autres fonctions de la bibliothèque strings pour effectuer des recherches et des remplacements plus avancés, tels que `Replace`, `ReplaceAllFunc` ou `ReplaceFirst`.

# Plongée en profondeur

En plus de la bibliothèque strings, Go offre également une bibliothèque regexp (expressions régulières), qui vous permet d'effectuer des recherches et des remplacements basés sur des modèles spécifiques. Par exemple, si vous voulez remplacer toutes les lettres minuscules par des majuscules dans une chaîne, vous pouvez utiliser le code suivant :

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    str := "Hello world!"

    // Remplace toutes les lettres minuscules par des majuscules
    re := regexp.MustCompile("[a-z]")
    newStr := re.ReplaceAllStringFunc(str, func(s string) string {
        return strings.ToUpper(s)
    })

    fmt.Println(newStr)
}
```

La sortie sera : "HELLO WORLD!". Vous pouvez explorer les nombreuses possibilités offertes par la bibliothèque regexp pour effectuer des recherches et des remplacements complexes en utilisant des expressions régulières.

# Voir aussi

- [La bibliothèque strings en Go](https://golang.org/pkg/strings/)
- [La bibliothèque regexp en Go](https://golang.org/pkg/regexp/)
- [Un guide complet sur les expressions régulières en Go](https://www.rexegg.com/regex-go.html)