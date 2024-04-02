---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:25.888371-07:00
description: "Commencer un nouveau projet en Go implique de configurer un espace de\
  \ travail et de l'initialiser avec les modules Go n\xE9cessaires. Les programmeurs\
  \ font\u2026"
lastmod: '2024-03-13T22:44:57.134166-06:00'
model: gpt-4-0125-preview
summary: "Commencer un nouveau projet en Go implique de configurer un espace de travail\
  \ et de l'initialiser avec les modules Go n\xE9cessaires. Les programmeurs font\u2026"
title: "D\xE9marrer un nouveau projet"
weight: 1
---

## Quoi et Pourquoi ?

Commencer un nouveau projet en Go implique de configurer un espace de travail et de l'initialiser avec les modules Go nécessaires. Les programmeurs font cela pour organiser le code, gérer efficacement les dépendances et faciliter les processus de build. C'est fondamental pour créer un logiciel scalable et maintenable en Go.

## Comment faire :

D'abord, assurez-vous d'avoir Go installé en exécutant `go version` dans votre terminal. Vous devriez voir la version de Go que vous avez installée en sortie. Ensuite, commençons un nouveau projet. Navigatez jusqu'à votre espace de travail et exécutez :

```shell
mkdir hello-world
cd hello-world
```

Cela crée et vous déplace dans un nouveau répertoire pour votre projet. Maintenant, initialisez le module :

```shell
go mod init example.com/hello-world
```

Remplacez `example.com/hello-world` par le chemin de votre module. Cette commande crée un fichier `go.mod` dans votre répertoire, signalant le début d'un nouveau module Go. Voici à quoi pourrait ressembler `go.mod` :

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` suit les dépendances de votre projet. Maintenant, créez un fichier `main.go` :

```shell
touch main.go
```

Ouvrez `main.go` dans votre éditeur préféré et ajoutez le code suivant pour imprimer "Hello, World!" :

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

Pour exécuter votre programme, revenez au terminal et exécutez :

```shell
go run main.go
```

Vous devriez voir :

```plaintext
Hello, World!
```

Félicitations ! Vous venez de commencer un nouveau projet Go et de lancer votre premier programme Go.

## Approfondissement

L'initiative d'introduire les modules comme standard pour la gestion des dépendances en Go a marqué un changement significatif dans l'écosystème de Go, officiellement adoptée dans Go 1.11. Avant les modules, les développeurs Go s'appuyaient sur la variable d'environnement GOPATH pour gérer les dépendances, ce qui était moins intuitif et menait souvent à l'infâme "enfer des dépendances".

Les modules offrent une manière encapsulée de gérer les dépendances du projet, la versioning, et marquent un pas vers la rendre les projets Go plus autocontenus et portables. Chaque module spécifie ses dépendances que Go suit dans le fichier `go.mod`, simplifiant la gestion des dépendances à travers différents environnements et stades de développement.

Cependant, il est à noter que bien que les modules Go soient maintenant la norme, certains projets hérités pourraient encore utiliser GOPATH. Pour la plupart des nouveaux projets, les modules offrent un système de gestion plus simple et efficace, mais comprendre GOPATH peut être utile pour maintenir ou contribuer à des codebases Go plus anciennes.

En termes d'alternatives, bien que les modules Go soient maintenant le standard de facto, la communauté Go a expérimenté avec d'autres outils de gestion des dépendances comme `dep` dans le passé. Cependant, ceux-ci ont été largement supplantés par le support officiel des modules intégré dans la chaîne d'outils Go.
