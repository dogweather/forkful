---
title:                "Lecture des arguments de ligne de commande"
date:                  2024-01-20T17:56:03.294235-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Lire les arguments de la ligne de commande, c'est récupérer les données saisies par l'utilisateur lorsqu'il lance votre programme. Les développeurs font ça pour personnaliser l'exécution du programme ou préciser des options sans interaction supplémentaire.

## How to: (Comment faire :)

Go offre le package `os`, simple et direct, pour accéder aux arguments de la ligne de commande. Voici un exemple rapide :

```go
package main

import (
	"fmt"
	"os"
)

func main() {
	// os.Args fournit tous les arguments de ligne de commande, y compris le nom du programme.
	args := os.Args[1:] // ignorer le nom du programme

	for _, arg := range args {
		fmt.Println(arg)
	}
}
```

Si on lance `go run main.go salut monde`, le résultat sera :

```
salut
monde
```

## Deep Dive (Plongée en profondeur)

Historiquement, lire les arguments de ligne de commande est aussi vieux que les premiers systèmes d'exploitation à interface texte. C'est la base de l'interaction utilisateur/programme.

En Go, `os.Args` est un slice contenant les arguments de la ligne de commande, où `os.Args[0]` est habituellement le chemin du programme exécuté. Pour des besoins plus avancés, comme le parsing d'options structurées (comme `-v` pour la version), Go dispose de packages comme `flag` ou des librairies tierces comme `cobra` ou `urfave/cli`.

Les arguments de la ligne de commande sont essentiels pour les scripts, les outils de CLI, ou quand on a besoin de passer des données simple rapidement sans UI graphique.

## See Also (Voir aussi)

- Documentation officielle de Go pour le package `os`: https://pkg.go.dev/os
- Documentation officielle de Go pour le package `flag`: https://pkg.go.dev/flag
- Package `cobra` pour les applications CLI en Go: https://github.com/spf13/cobra
- Package `urfave/cli` pour les applications CLI en Go: https://github.com/urfave/cli
