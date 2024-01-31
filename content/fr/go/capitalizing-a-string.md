---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Capitaliser une chaîne transforme les premières lettres de chaque mot en majuscules, un peu comme pour les titres. Cela aide à uniformiser les données en entrée et à les rendre visuellement agréables.

## Comment faire :
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	phrase := "le langage go est super!"
	phraseCapitalisee := strings.Title(strings.ToLower(phrase))
	fmt.Println(phraseCapitalisee) // Affiche : "Le Langage Go Est Super!"
}
```

## Exploration approfondie
Historiquement, capitaliser chaque mot d'une chaîne reflète des conventions typographiques, comme pour les titres de livres. En Go, `strings.Title()` fait le boulot, mais attention, il capitalise chaque premier caractère après un espace. Si vous avez besoin de respecter certaines règles typographiques particulières (comme celles des titres APA), envisagez une solution personnalisée. 

`strings.ToUpper()` transforme en majuscules, mais touche toute la chaîne, pas idéal pour les titres mais utile pour les acronymes ou quand la distinction de casse n'est pas voulue.

D'un point de vue implémentation, soyez conscients des langues qui utilisent des caractères non latins ou des cas spéciaux comme le turc, où la capitalisation ne suit pas les mêmes règles que l'anglais.

## Voir également
- `strings` package doc: https://pkg.go.dev/strings
- Article sur la typographie des titres: https://www.grammarly.com/blog/capitalization-in-the-titles/
- Unicode et capitalisation : https://blog.golang.org/strings
