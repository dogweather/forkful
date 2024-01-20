---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'interpolation de chaîne consiste à insérer des valeurs de variables dans une chaîne de caractères. Les programmeurs le font pour rendre le code plus lisible, faciliter le formatage de texte et éviter les erreurs causées par la concaténation complexe.

## Comment faire:
Voici un exemple simple d'interpolation de chaîne en Go. 

```Go
package main

import (
	"fmt"
)

func main() {
	name := "Jean"
	message := fmt.Sprintf("Bonjour, %s!", name)

	fmt.Println(message) 
}
```
Cela affiche:

```Go
Bonjour, Jean!
```

L'interpolation s'effectue en utilisant `Sprintf` de `fmt` et `%s` est le spécificateur de format pour une chaîne.

## Plongée plus profonde
Historiquement, Go n'a pas offert l'interpolation de chaînes comme d'autres langages. C'était intentionnel, afin d'encourager la clarté et de minimiser les erreurs. cependant, avec `Sprintf`, vous pouvez intercaler sans souci.

Les alternatives à l'interpolation incluent la concaténation (+) et la jointure (strings.Join). Cependant, elles peuvent devenir verbeuses et difficiles à lire. Par exemple:

```Go
name := "Jean"
message := "Bonjour, " + name + "!"
```
ou
```Go
parts := []string{"Bonjour, ", name, "!"}
message := strings.Join(parts, "")
```

Utiliser `Sprintf` est souvent plus clair, concis, et plus efficace en termes de mémoire.

## Voir aussi
Pour plus d'informations sur l'interpolation de chaîne en Go, consultez:
1. Documents officiels de Go: https://golang.org/pkg/fmt/
2. Écriture efficace avec `sprintf` : https://yourbasic.org/golang/fmt-printf-sprintf-fprintf-print-println/
3. Spécificateurs de format: https://dave.cheney.net/2019/07/09/annotating-log-strings.