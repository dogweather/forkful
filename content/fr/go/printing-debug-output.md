---
title:                "Afficher la sortie de débogage"
html_title:           "Go: Afficher la sortie de débogage"
simple_title:         "Afficher la sortie de débogage"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Imaginez que vous êtes en train de coder une nouvelle fonctionnalité pour votre application en Go. Tout se passe bien jusqu'à ce que vous rencontriez un bug. Vous vous retrouvez à fouiller dans votre code pour trouver l'endroit précis où le problème se produit. C'est là qu'entrent en jeu les sorties de débogage. Cet article va vous montrer pourquoi les sorties de débogage sont importantes et comment les utiliser efficacement en Go.

## Comment faire

En Go, il existe deux façons courantes de générer des sorties de débogage: la fonction `fmt.Println()` et la fonction `log.Print()`. Jetons un coup d'œil à un exemple de code utilisant ces fonctions:

```Go
package main

import (
	"fmt"
	"log"
)

func main() {
	nom := "Marie"
	age := 30

	fmt.Println("Nom:", nom)
	log.Print("Âge:", age)
}
```

Dans cet exemple, nous utilisons `fmt.Println()` pour imprimer le nom de la personne et `log.Print()` pour imprimer son âge. L'avantage de ces fonctions est qu'elles sont très simples à utiliser. Elles sont également utiles pour les débutants en Go car elles donnent des informations très détaillées sur les valeurs des variables.

Lorsque vous exécutez ce code, vous obtenez la sortie suivante:

```
Nom: Marie
Âge: 30
```

Vous pouvez également utiliser des chaînes de format pour afficher des valeurs de variables spécifiques. Par exemple:

```Go
age := 30

fmt.Printf("Mon âge est %d", age)
```

Cette ligne de code va afficher "Mon âge est 30". Vous pouvez également inclure plusieurs variables dans une seule chaîne de format en utilisant des arguments supplémentaires.

## Plongée en profondeur

Il existe plusieurs façons de personnaliser vos sorties de débogage en Go. Vous pouvez utiliser `fmt.Sprintf()` pour retourner une chaîne de format plutôt que de l'imprimer directement. Vous pouvez également utiliser `log.SetPrefix()` pour ajouter un préfixe à toutes vos sorties. De plus, vous pouvez utiliser `log.SetFlags()` pour modifier le format de la date et de l'heure dans vos sorties.

Une autre option intéressante est d'utiliser le package `logrus`. Ce package offre une bibliothèque beaucoup plus puissante pour la génération de sorties de débogage. Il vous permet de personnaliser complètement vos sorties et de les enregistrer dans des fichiers, des bases de données ou même des services cloud tels que AWS CloudWatch.

## Voir aussi

- [Documentation officielle Go pour les fonctions de formatage et de débogage](https://golang.org/pkg/fmt/)
- [Documentation officielle Go pour le package log](https://golang.org/pkg/log/)
- [Package logrus pour les sorties de débogage avancées](https://github.com/sirupsen/logrus)