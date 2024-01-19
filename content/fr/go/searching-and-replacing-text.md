---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?

Chercher et remplacer du texte, c'est naviguer dans un texte pour trouver un motif spécifique et le remplacer par un autre. Les programmeurs le font pour modifier le contenu des fichiers textuels, pour corriger les erreurs ou pour automatiser les tâches de routine.

## Comment faire:

Voici un exemple simple en utilisant la fonction `strings.Replace` de la bibliothèque standard Go.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Bonjour, le monde!"
    newStr := strings.Replace(str, "le", "mon", -1)
    fmt.Println(newStr)
}
```

Cet exemple va imprimer: "Bonjour, mon monde!". Le "le" dans notre chaine a été remplacé par "mon".

## Plongée Profonde:

Historiquement, la recherche et le remplacement de texte étaient des opérations de base réalisées à l'aide d'outils de ligne de commande Unix comme `sed`. Go permet la même fonctionnalité avec l'ajout de la facilité d'utilisation et de la performance.

Il existe des alternatives à `strings.Replace`. Par exemple, le package `regexp` permet une recherche et un remplacement plus avancés à l'aide d'expressions régulières.

L'implémentation de la recherche et du remplacement en Go est très efficace. Dans `strings.Replace`, Go cherche les occurrences du vieux fragment dans la chaîne, les remplace par le nouveau fragment, et recrée la chaîne.

## Voir Aussi:

Pour une utilisation plus avancée, vous pouvez consulter la documentation officielle de Go pour les packages [strings](https://golang.org/pkg/strings/) et [regexp](https://golang.org/pkg/regexp/). Un guide d'introduction sur les expressions régulières en Go peut être trouvé [ici](https://www.calhoun.io/regular-expressions-in-go/). Vous pouvez également consulter ce [tutoriel](https://www.tutorialspoint.com/go/go_strings.htm) sur Go Strings.