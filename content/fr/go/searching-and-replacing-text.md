---
title:                "Rechercher et remplacer du texte"
html_title:           "Go: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Go, vous savez sans doute l'importance de la gestion des chaînes de caractères dans vos programmes. L'une des tâches essentielles liées à cette gestion est la recherche et le remplacement de texte. Dans cet article, nous allons explorer comment effectuer cette opération en utilisant Go.

## Comment faire

La méthode la plus simple pour rechercher et remplacer du texte en Go est d'utiliser la fonction strings.Replace(). Voici un exemple de code pour remplacer toutes les occurrences d'un mot par un autre :

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Bonjour le monde!"
    newStr := strings.Replace(str, "Bonjour", "Salut", -1)
    fmt.Println(newStr)
}

// Output : Salut le monde!
```

Dans cet exemple, nous avons utilisé la fonction strings.Replace() en passant en paramètre la chaîne de caractères à modifier, le texte à remplacer et le texte de remplacement. Le dernier paramètre est " -1" pour indiquer que nous souhaitons remplacer toutes les occurrences.

Vous pouvez également utiliser la fonction strings.ReplaceAll() si vous utilisez la version 1.15 de Go. Elle fonctionne de la même manière que strings.Replace() mais elle remplace toutes les occurrences sans avoir besoin de spécifier "-1" comme dernier paramètre.

Si vous souhaitez rechercher et remplacer uniquement la première occurrence, vous pouvez utiliser la fonction strings.ReplaceOne() ou utiliser un index pour spécifier l'emplacement précis où effectuer le remplacement.

Il est également possible d'utiliser des expressions régulières pour des recherches plus avancées en utilisant le package regexp.

## Plongée en profondeur

La fonction strings.Replace() utilise un algorithme de remplacement simple basé sur une boucle de recherche et de remplacement dans la chaîne de caractères. Si vous devez effectuer de nombreuses opérations de remplacement, il peut être plus efficace d'utiliser le package bytes et sa fonction ReplaceAll() pour travailler directement avec les octets plutôt qu'avec les chaînes de caractères.

Il est important de noter que la fonction strings.Replace() renvoie une nouvelle chaîne de caractères modifiée plutôt que de modifier directement la chaîne d'origine. Si vous souhaitez modifier la chaîne d'origine, vous devez utiliser la fonction strings.ReplaceAll() (disponible en version 1.12 ou supérieure).

## Voir aussi

- [Documentation officielle sur la gestion des chaînes de caractères en Go](https://golang.org/pkg/strings)
- [Guide des expressions régulières en Go](https://yourbasic.org/golang/regexp-cheat-sheet)
- [Article sur les différentes méthodes de recherche et de remplacement en Go](https://flaviocopes.com/golang-replace/)