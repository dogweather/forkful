---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Supprimer les caractères correspondant à un modèle (ou "pattern") signifie éliminer tous les caractères d'une chaîne qui correspondent à un modèle spécifié. Les programmeurs le font pour nettoyer et structurer les données de manière plus efficace.

## Comment faire:
Voici un exemple simple en utilisant la fonction `Replace` du package `strings` de Go pour supprimer tous les 'a' d'une chaîne.

```Go
package main
import (
    "fmt"
    "strings"
)

func main() {
    str := "abracadabra"
    newStr := strings.Replace(str, "a", "", -1)
    fmt.Println(newStr) // "brcdbr"
}
```
L'exécution de ce code restituera "brcdbr", puisque tous les 'a' ont été supprimés.

## Plongée profonde
Historiquement, l'expression régulière est le moyen le plus courant de correspondre à un motif dans une chaîne, mais en Go, la librairie `strings` fournit des outils plus faciles et plus efficaces. Une alternative pourrait être `strings.Map`, si vous devez supprimer plusieurs caractères différents à la fois. `strings.Replace` fonctionne en analysant chaque caractère de la chaîne, ce qui signifie que la complexité de l'opération est O(n), où n est la longueur de la chaîne.

## Voir aussi
Pour plus de détails sur les fonctions de Go pour manipuler les chaînes, visitez [Go Docs - Package strings](https://golang.org/pkg/strings/). Pour une introduction aux expressions régulières en Go, consultez [Go by Example - Regular Expressions](https://gobyexample.com/regular-expressions).