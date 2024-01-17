---
title:                "Ecrire sur la sortie d'erreur standard"
html_title:           "Go: Ecrire sur la sortie d'erreur standard"
simple_title:         "Ecrire sur la sortie d'erreur standard"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font?

Ecrire vers la sortie d'erreur standard est une méthode utilisée par les programmeurs pour afficher des informations sur les erreurs ou les avertissements dans leur code. Cela peut être utile pour déboguer et identifier les problèmes dans un programme.

## Comment faire:

Voici un exemple de code en Go qui utilise la fonction "Fprint", qui permet d'écrire du texte dans la sortie d'erreur standard:

```
package main

import (
  "fmt"
  "io"
  "os"
)

func main() {
  // Crée une variable contenant le texte à écrire dans la sortie d'erreur
  msg := "Attention, une erreur s'est produite!"
  
  // Utilise la fonction "Fprint" pour écrire le message dans la sortie d'erreur standard
  _, err := fmt.Fprint(os.Stderr, msg)
  
  if err != nil {
    // En cas d'erreur, affiche la description de l'erreur
    fmt.Println("Une erreur s'est produite:", err)
  }
}
```

Lorsque vous exécutez ce code, vous devriez obtenir le message d'erreur "Attention, une erreur s'est produite!" dans la sortie d'erreur standard, qui est généralement la console ou le terminal.

## Plongée en profondeur:

L'écriture vers la sortie d'erreur standard a été introduite dans le langage de programmation C, et de nombreux autres langages ont également adopté cette méthode. Cependant, il existe d'autres alternatives, telles que l'utilisation de fichiers journaux ou de bibliothèques de gestion des erreurs.

La fonction "Fprint" utilisée dans l'exemple ci-dessus est seulement une des nombreuses fonctions disponibles dans la bibliothèque "fmt" de Go qui permettent d'écrire vers la sortie d'erreur standard. Vous pouvez également utiliser "Fprintf" pour formater le texte avant de l'écrire, ou "Fprintln" pour ajouter une nouvelle ligne à la fin du message.

## Voir aussi:

- [Documentation officielle de la fonction "Fprint" de Go](https://golang.org/pkg/fmt/#Fprint)
- [Article sur les méthodes de gestion des erreurs en Go](https://blog.golang.org/defer-panic-and-recover)
- [Exemple de code utilisant la sortie d'erreur standard dans un programme d'enregistrement de fichiers journaux en Go](https://github.com/vjeantet/log/blob/master/log.go)