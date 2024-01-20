---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Démarrer un nouveau projet en programmation Go c'est comme dessiner un plan pour un bâtiment. C'est essentiel car sans cela, nous ne saurions pas où aller et comment le rendre réel.

## Comment faire:
Voici comment vous pouvez initialiser un nouveau projet Go :

```Go
package main

import "fmt"

func main() {
    fmt.Println("Bonjour, nouveau projet!")
}
```

Exécutez le fichier, vous verrez ce qui suit en sortie :

```Go
Bonjour, nouveau projet!
```

## Plongée en profondeur
Historiquement, les projets Go étaient tous dans un même espace de travail ce qui causaient des conflits de dépendances. Vers Go 1.11, "Go modules" est devenu la nouvelle norme pour la gestion de projet - ses concurrents étant GB et Godep. Les modules ont permis d'avoir de multiples versions d'un même package dans un même espace de travail.

## Voir Également
2. [Gérer les dépendances avec Go modules](https://blog.golang.org/using-go-modules)
3. [Initier un nouveau projet Go avec VSCode](https://medium.com/@san40u/setting-up-golang-in-vscode-8ea79b2106fb)