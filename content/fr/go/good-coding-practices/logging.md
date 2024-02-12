---
title:                "Journalisation"
aliases: - /fr/go/logging.md
date:                  2024-02-03T17:59:03.211545-07:00
model:                 gpt-4-0125-preview
simple_title:         "Journalisation"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/logging.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Le journalisation dans le développement logiciel est le processus d'enregistrement d'informations sur l'exécution d'un programme, conçu pour suivre son comportement et diagnostiquer les problèmes. Les programmeurs mettent en œuvre la journalisation pour surveiller les performances du logiciel, déboguer les erreurs et garantir la sécurité et la conformité du système, en faisant un outil indispensable pour la maintenance et l'analyse des applications.

## Comment :

En Go, la journalisation peut être mise en œuvre en utilisant le package de la bibliothèque standard `log`. Ce package fournit des capacités de journalisation simples, telles que l'écriture sur la sortie standard ou dans des fichiers. Commençons par un exemple de base de journalisation sur la sortie standard :

```go
package main

import (
	"log"
)

func main() {
	log.Println("Ceci est une entrée de journal basique.")
}
```

Sortie :
```
2009/11/10 23:00:00 Ceci est une entrée de journal basique.
```

Le timestamp au début de l'entrée de journal est automatiquement ajouté par le package `log`. Ensuite, explorons comment consigner dans un fichier au lieu de la sortie standard :

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("Cette entrée de journal va dans un fichier.")
}
```

Maintenant, mettons en œuvre un cas d'utilisation plus avancé : personnaliser le format de journalisation. Go vous permet de créer un journaliseur personnalisé avec `log.New()` :

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "JOURNAL PERSONNALISÉ : ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("Ceci est un message de journal personnalisé.")
}
```

Sortie :
```
JOURNAL PERSONNALISÉ : 2009/11/10 23:00:00 main.go:11: Ceci est un message de journal personnalisé.
```

Cet exemple préfixe chaque message de journal avec "JOURNAL PERSONNALISÉ : " et inclut la date, l'heure et l'emplacement du fichier source.

## Approfondissement

Le package `log` de la bibliothèque standard de Go est simple et suffisant pour de nombreuses applications, mais il manque certaines des fonctionnalités plus sophistiquées trouvées dans les bibliothèques de journalisation tierces, telles que la journalisation structurée, la rotation des journaux et la journalisation basée sur le niveau. Des packages comme `zap` et `logrus` offrent ces fonctionnalités avancées et sont bien considérés dans la communauté Go pour leur performance et leur flexibilité.

La journalisation structurée, par exemple, vous permet de consigner des données dans un format structuré (comme le JSON), ce qui est particulièrement utile pour les applications modernes basées sur le cloud où les journaux peuvent être analysés par divers outils ou services. `zap`, en particulier, est connu pour sa haute performance et sa faible surcharge d'allocation, le rendant adapté aux applications où la vitesse et l'efficacité sont critiques.

Historiquement, la journalisation en Go a évolué de manière significative depuis l'inception du langage. Les premières versions de Go fournissaient les capacités de journalisation de base que nous voyons dans le package `log`. Cependant, à mesure que le langage gagnait en popularité et que la complexité des applications écrites en Go augmentait, la communauté a commencé à développer des bibliothèques de journalisation plus sophistiquées pour répondre à leurs besoins. Aujourd'hui, alors que le package `log` standard reste une option viable pour des applications simples, de nombreux développeurs se tournent vers ces solutions tierces pour des exigences de journalisation plus complexes.
