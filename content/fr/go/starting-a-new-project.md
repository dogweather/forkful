---
title:                "Go: Commencer un nouveau projet"
simple_title:         "Commencer un nouveau projet"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous cherchez un langage de programmation rapide, fiable et facile à apprendre, alors Go est fait pour vous. Avec sa syntaxe simple et épurée, Go est idéal pour les débutants en programmation ainsi que pour les développeurs expérimentés. Dans cet article, nous allons explorer comment démarrer un nouveau projet en utilisant le langage Go.

## Comment Faire

Tout d'abord, assurez-vous d'avoir Go installé sur votre ordinateur. Vous pouvez vérifier si Go est déjà installé en exécutant la commande `go version` dans votre terminal. Si ce n'est pas le cas, vous pouvez télécharger Go à partir du site officiel et suivre les instructions d'installation.

Une fois que vous avez Go installé, vous pouvez commencer à créer votre projet. Tout d'abord, vous devez créer un dossier pour votre projet et vous y rendre dans votre terminal. Ensuite, exécutez la commande `go mod init nomduprojet`, en remplaçant "nomduprojet" par le nom que vous souhaitez donner à votre projet. Cela créera un fichier go.mod dans votre dossier, qui sera utilisé pour suivre les dépendances de votre projet.

Maintenant que votre projet est initialisé, vous pouvez commencer à coder. Voici un exemple de code qui affiche "Bonjour, monde!" dans la console :

```Go
package main

import "fmt"

func main() {
    fmt.Println("Bonjour, monde!")
}
```

Pour exécuter ce code, vous pouvez utiliser la commande `go run chemin/vers/votre/fichier.go` dans votre terminal. Vous devriez voir "Bonjour, monde!" s'afficher.

## Plongée en Profondeur

Lorsque vous démarrez un nouveau projet en utilisant Go, il est important de se familiariser avec la structure de projet recommandée. Normalement, les projets Go ont un dossier src (pour le code source), un dossier bin (pour les fichiers binaires) et un dossier pkg (pour les dépendances). Il est également courant de trouver un fichier README.md à la racine du projet, qui contient des informations sur le projet et son utilisation.

Il est également important de noter que Go suit une convention de nommage pour les packages et les variables. Les noms de packages doivent être tous en minuscules et les noms de variables peuvent être en minuscules ou en camelCase. Respecter cette convention rend votre code plus lisible pour les autres développeurs et facilite la collaboration.

Enfin, n'oubliez pas d'utiliser les outils fournis par Go pour gérer les dépendances de votre projet. Vous pouvez utiliser la commande `go get` pour installer de nouvelles dépendances et la commande `go mod tidy` pour mettre à jour les dépendances existantes.

## Voir Aussi

- [Site officiel de Go](https://golang.org)
- [Documentation de Go](https://golang.org/doc/)
- [Tour de Go interactive](https://tour.golang.org)