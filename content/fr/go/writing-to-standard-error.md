---
title:                "Écrire vers les erreurs standards"
html_title:           "Go: Écrire vers les erreurs standards"
simple_title:         "Écrire vers les erreurs standards"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi écrire vers l'erreur standard ?

Avez-vous déjà rencontré une erreur dans votre code et vous ne pouvez pas le comprendre ? Écrire vers l'erreur standard peut vous aider à identifier et résoudre ces erreurs plus facilement en affichant des messages d'erreur spécifiques.

## Comment faire

Dans Go, vous pouvez écrire vers l'erreur standard en utilisant la fonction `fmt.Fprintf()` ou `fmt.Fprintln()` en spécifiant `os.Stderr` comme premier argument. Voici un exemple de code :

```
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprintf(os.Stderr, "Il y a une erreur dans votre code !")
}
```

Lorsque vous exécutez ce code, vous obtiendrez le message d'erreur affiché dans la sortie d'erreur standard : "Il y a une erreur dans votre code !".

## Deep Dive

En écrivant vers l'erreur standard, vous pouvez également spécifier des codes de statut pour indiquer quelle type d'erreur s'est produite. Par exemple, en utilisant `fmt.Fprintln()` avec le code de statut `os.Exit(1)`, vous pouvez signaler une erreur fatale et arrêter l'exécution du programme. Voici un exemple de code :

```
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprintln(os.Stderr, "Erreur fatale !")
    os.Exit(1)
}
```

Vous pouvez également écrire des informations de débogage dans l'erreur standard en utilisant `fmt.Fprintf()` avec le format `%v` qui affiche une valeur sous forme de chaîne de caractères. Par exemple :

```
package main

import (
    "fmt"
    "os"
)

func main() {
    age := 30
    fmt.Fprintf(os.Stderr, "L'âge est de %v ans.", age)
}
```

Cela affichera "L'âge est de 30 ans." dans la sortie d'erreur standard.

## Voir aussi

- Documentation officielle de Go sur les sorties standards : https://golang.org/pkg/os/#package-variables
- Un tutoriel sur l'utilisation de fmt.Fprintf() : https://www.golangprograms.com/how-to-write-data-to-standard-error-in-golang.html