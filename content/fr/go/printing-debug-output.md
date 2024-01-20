---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La sortie de débogage est un outil essentiel dans le développement de logiciels qui permet aux programmeurs de suivre le déroulement d'un programme et de localiser les erreurs. Les programmeurs l'utilisent pour résoudre les bugs et optimiser les performances.

## Comment :

Voici un exemple de base de comment imprimer la sortie de débogage en Go :

```Go
package main

import (
	"log"
	"os"
)

func main() {
	log.SetOutput(os.Stdout)
	log.Print("C'est un message de débogage.")
}

```

Quand vous exécutez ce code, vous verrez le message de débogage suivant :

```
2021/07/15 16:23:43 C'est un message de débogage.
```

## Plongée en Profondeur :

Historiquement, le débogage a été utilisé depuis les débuts de la programmation pour résoudre les problèmes de logiciel. En Go, la bibliothèque standard 'log' est couramment utilisée pour ce faire. Mais il y a aussi des alternatives comme `logrus` ou `zerolog` qui offrent plus de fonctionnalités.

Lors de l'impression de la sortie de débogage en Go, la fonction `log.Print` écrit dans un io.Writer que vous définissez. Par défaut, c'est os.Stderr, mais vous pouvez changer cela avec `log.SetOutput`.

## Voir aussi :

Pour plus d'informations sur la bibliothèque log en Go, consultez la [Documentation Officielle](https://golang.org/pkg/log/).

Si vous êtes intéressé par des alternatives plus fonctionnelles, voici quelques liens vers leurs pages GitHub : [`logrus`](https://github.com/sirupsen/logrus), [`zerolog`](https://github.com/rs/zerolog).

C'est tout. Amusez-vous à écrire du code et à déboguer !