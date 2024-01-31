---
title:                "Lancement d'un nouveau projet"
date:                  2024-01-20T18:03:23.763577-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lancement d'un nouveau projet"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Commencer un nouveau projet en Go, c'est initialiser l'espace de travail avec les fichiers de base. Les programmeurs font ça pour organiser et préparer le terrain pour leur code futur.

## Comment Ça Marche :

```Go
package main

import "fmt"

func main() {
    fmt.Println("Bonjour le nouveau projet Go!")
}
```

Sortie attendue :
```
Bonjour le nouveau projet Go!
```

Initialisation du projet :
```shell
$ mkdir monprojetgo
$ cd monprojetgo
$ go mod init monprojetgo
```

Après l'initialisation, vous pouvez créer des fichiers `.go` et commencer à coder.

## Des Infos Plus Poussées

Historiquement, Go (ou Golang) a été créé par Google pour simplifier la programmation système. En structurant un nouveau projet, on établit les dépendances avec `go mod` depuis Go 1.11. C'est plus propre que les anciens GOPATH. 

Alternatives ? Certains utilisent des outils comme GoLand ou Visual Studio Code avec des extensions pour démarrer un projet, mais la ligne de commande reste roi pour la simplicité et contrôle.

Côté implémentation, la création d'un `go.mod` définit les modules et leurs versions. On gère ainsi mieux les paquets utilisés, ce qui rend les builds répétables et les partages de projet plus fluides. Plus de "ça marche sur ma machine" !

## Voir Aussi

- La documentation officielle de Go: https://golang.org/doc/
- Un tutoriel approfondi sur `go mod`: https://blog.golang.org/using-go-modules
- Un guide pour bien structurer vos projets Go: https://github.com/golang-standards/project-layout
