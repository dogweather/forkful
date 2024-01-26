---
title:                "Retirer les guillemets d'une chaîne"
date:                  2024-01-26T03:39:20.673867-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & pourquoi ?

Enlever les guillemets d'une chaîne signifie se débarrasser de ces caractères de guillemets doubles ou simples encombrants qui enveloppent votre texte réel. Nous faisons cela pour assainir les données, prévenir les erreurs d'analyse, ou préparer le texte pour un traitement ultérieur sans le fioritures des guillemets.

## Comment faire :

Voici la manière simple de mettre les guillemets à la porte en Go :

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Bonjour, le monde!\""
	fmt.Println("Original :", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Sans guillemets :", unquotedString)
}
```

Le résultat ressemblera à cela, les guillemets tous partis :

```
Original : "Bonjour, le monde!"
Sans guillemets : Bonjour, le monde!
```

## Plongée profonde

Autrefois, quand les formats de données et l'échange n'étaient pas standardisés, les guillemets dans les chaînes pouvaient causer le chaos. Ils le peuvent toujours, surtout dans JSON ou lors de l'insertion de chaînes dans des bases de données. Le package `strings` en Go est équipé d'une fonction `Trim`, qui élimine non seulement les espaces blancs mais aussi tout caractère qui ne vous plaît pas.

Pourquoi pas les Regex ? Eh bien, `Trim` est plus rapide pour les travaux simples, mais si vos chaînes jouent à cache-cache avec des guillemets dans des endroits bizarres, regex pourrait être votre artillerie lourde :

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

C'est comme choisir entre des ciseaux et une tronçonneuse ; choisissez l'outil adapté au travail.

## Voir aussi

Pour en savoir plus sur le package `strings` et ses outils puissants :
- [Package strings](https://pkg.go.dev/strings)

Pour manier la puissance des expressions régulières en Go :
- [Package regexp](https://pkg.go.dev/regexp)

Envie de plonger dans la philosophie de la coupe des chaînes ?
- [La méthode Trim](https://blog.golang.org/strings)