---
title:                "Extraction de sous-chaînes"
date:                  2024-01-20T17:45:46.233723-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Extraire des sous-chaînes, c'est obtenir des parties spécifiques d'une chaîne de caractères. On fait ça pour manipuler et utiliser des bouts de texte sans toucher au reste, par exemple, pour analyser des données ou pour afficher seulement ce qui est nécessaire.

## Comment faire :

```go
package main

import (
	"fmt"
)

func main() {
	texte := "Bonjour, je suis un développeur Go!"
	
	// Extraire "Bonjour"
	substring1 := texte[:7]
	fmt.Println(substring1) // Affiche "Bonjour"
	
	// Extraire "développeur"
	substring2 := texte[17:28]
	fmt.Println(substring2) // Affiche "développeur"
	
	// Extraire "Go"
	indiceDebut := len(texte)-3
	substring3 := texte[indiceDebut:]
	fmt.Println(substring3) // Affiche "Go!"
}
```
Sortie :
```
Bonjour
développeur
Go!
```

## Exploration en profondeur

Historiquement, l'extraction de sous-chaînes fait partie des opérations essentielles en programmation. Depuis les débuts de la programmation, manipuler le texte c'est souvent traiter chaque partie individuellement. En Go, cela se fait par les indices des slices (tranches) qui sont basés sur les tableaux. Les indices commencent à 0.

Il y a des alternatives. Par exemple, `strings` est un package qui offre `strings.Split` pour découper par un séparateur, ou `strings.Index` pour trouver un sous-texte et l'extraire. Il faut faire attention avec les runes et les bytes : en Go, une `string` est une suite de bytes, donc si tu manipules des caractères non-ASCII, tu pourrais avoir besoin de `[]rune` pour éviter de couper en plein milieu d'un caractère.

## Voir Également

- Documentation officielle de Go pour le package `strings`: https://golang.org/pkg/strings/
- Tour de Go pour comprendre les slices : https://tour.golang.org/moretypes/7
- Article Go blog sur les chaînes, les bytes et les runes : https://blog.golang.org/strings
