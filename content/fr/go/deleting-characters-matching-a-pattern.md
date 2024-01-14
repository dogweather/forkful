---
title:                "Go: Supprimer les caractères correspondant à un motif"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

De temps en temps, vous pourriez vous retrouver dans une situation où vous avez besoin de supprimer certains caractères d'une chaîne de caractères en Go. Cela peut sembler être une tâche simple, mais il y a des cas où cela peut être très utile dans le développement d'applications. Dans cet article, nous allons explorer comment supprimer des caractères qui correspondent à un modèle spécifique en Go.

## Comment faire

Pour supprimer des caractères qui correspondent à un modèle en Go, nous allons utiliser la fonction `ReplaceAllString` du paquet `regexp`. Cette fonction prend deux arguments : la chaîne de caractères à modifier et le modèle à rechercher. Voyons un exemple concret :

```
package main

import (
  "fmt"
  "regexp"
)

func main() {
  str := "Bonjour! Comment allez-vous ?"

  // Le modèle que nous voulons supprimer
  pattern := "[? !]"

  // Utilisation de ReplaceAllString pour supprimer les caractères correspondants
  newStr := regexp.MustCompile(pattern).ReplaceAllString(str, "")

  fmt.Println(newStr) // Devrait imprimer "BonjourCommentallezvous"
}
```

Comme vous pouvez le voir, en utilisant la fonction `ReplaceAllString` et le modèle approprié, nous avons pu supprimer facilement les caractères qui correspondent à ce modèle dans notre chaîne de caractères.

## Plongée en profondeur

La fonction `ReplaceAllString` fonctionne en utilisant les expressions régulières. Si vous n'êtes pas familier avec les expressions régulières, elles sont des modèles utilisés pour rechercher et remplacer des motifs dans du texte. En utilisant différents modèles, vous pouvez supprimer des caractères spécifiques d'une chaîne de caractères en Go.

Il est également important de noter que la fonction `ReplaceAllString` remplace toutes les occurrences du modèle dans la chaîne de caractères. Si vous ne voulez supprimer qu'une seule occurrence, vous pouvez utiliser la fonction `ReplaceAllStringN` en spécifiant le nombre d'occurrences à remplacer.

## Voir aussi

- https://golang.org/pkg/regexp/
- https://yourbasic.org/golang/regexp-cheat-sheet/
- https://www.geeksforgeeks.org/how-to-delete-all-occurrences-of-a-string-in-go/