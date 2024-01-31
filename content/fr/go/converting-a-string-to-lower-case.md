---
title:                "Conversion d'une chaîne de caractères en minuscules"
date:                  2024-01-20T17:38:23.934468-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une chaîne de caractères en minuscules"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? | Quoi & Pourquoi ?
Transformer une chaîne de caractères en minuscules, c'est simplement changer tous les caractères majuscules en leurs équivalents minuscules. C'est utile pour uniformiser les données, par exemple, avant de les comparer ou de les stocker.

## How to: | Comment faire :
```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Bonjour, Go!"
	strLower := strings.ToLower(str)
	fmt.Println(strLower)
}
```
Sortie :
```
bonjour, go!
```

## Deep Dive | Plongée en profondeur
Avant Unicode, il suffisait de changer la casse des 26 lettres de l'alphabet anglais. Aujourd'hui, `strings.ToLower` dans Go utilise l'Unicode pour supporter toutes les langues. Des alternatives ? `bytes.ToLower` pour travailler directement avec des slices d'octets. Côté implémentation, Go utilise un mapping de caractères pour transformer majuscules en minuscules, ce qui prend en compte les spécificités de chaque langue.

## See Also | Voir Aussi
- La documentation officielle de Go pour `strings.ToLower`: https://pkg.go.dev/strings#ToLower
- Unicode et le changement de casse: https://unicode.org/faq/casemap_charprop.html
- Comparaisons et tri de chaînes en Go: https://blog.golang.org/strings
