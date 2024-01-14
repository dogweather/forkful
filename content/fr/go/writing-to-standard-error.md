---
title:    "Go: Ecrire vers la sortie d'erreur standard"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi écrire vers l'erreur standard en Go ?

Écrire vers l'erreur standard peut être utile lorsque vous déboguez votre code ou lorsque vous voulez afficher des messages aux utilisateurs de votre programme. Cela peut également être utile pour suivre l'avancement d'une tâche en cours d'exécution.

## Comment faire pour écrire vers l'erreur standard en Go ?

Dans Go, vous pouvez écrire vers l'erreur standard en utilisant la fonction `fmt.Fprintln()`. Voici un exemple de code et sa sortie :

```Go
package main

import "fmt"

func main() {
    fmt.Fprintln(os.Stderr, "Ceci est un exemple de message d'erreur standard.")
}
```

Sortie :

```
Ceci est un exemple de message d'erreur standard.
```

Vous pouvez également utiliser la méthode `os.Stderr.WriteString()` pour écrire directement dans l'erreur standard.

## Plongée en profondeur

Lorsque vous écrivez vers l'erreur standard, il est important de noter que le texte sera affiché en rouge dans les terminaux utilisant la coloration ANSI. De plus, vous pouvez également utiliser les verbes de formatage de `fmt.Sprintf()` pour formater vos messages avant de les écrire vers l'erreur standard. Cela peut être utile pour afficher des variables ou des messages dynamiques.

## Voir aussi

- [Documentation officielle de la fonction fmt.Fprintln()](https://golang.org/pkg/fmt/#Fprintln)
- [Documentation officielle de la méthode os.Stderr.WriteString()](https://golang.org/pkg/os/#Stderr)
- [Guide de formatage avec fmt.Sprintf()](https://gobyexample.com/string-formatting)