---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:37.703598-07:00
description: "Lire un fichier texte en Go implique d'acc\xE9der et de r\xE9cup\xE9\
  rer le contenu d'un fichier stock\xE9 sur disque pour le traiter ou l'analyser.\
  \ Les programmeurs\u2026"
lastmod: '2024-03-13T22:44:57.153744-06:00'
model: gpt-4-0125-preview
summary: "Lire un fichier texte en Go implique d'acc\xE9der et de r\xE9cup\xE9rer\
  \ le contenu d'un fichier stock\xE9 sur disque pour le traiter ou l'analyser."
title: Lire un fichier texte
weight: 22
---

## Comment faire :
Lire un fichier texte en Go peut être accompli de plusieurs manières, mais l'une des méthodes les plus simples est d'utiliser le paquet `ioutil`. Voici un exemple basique :

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
)

func main() {
    content, err := ioutil.ReadFile("exemple.txt")
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(string(content))
}
```

En supposant que `exemple.txt` contient "Bonjour, Go !", ce programme afficherait :

```
Bonjour, Go !
```

Cependant, depuis Go 1.16, le paquet `ioutil` a été déprécié, et il est recommandé d'utiliser les paquets `os` et `io` à la place. Voici comment vous pouvez accomplir la même chose avec ces paquets :

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    file, err := os.Open("exemple.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```

Cette approche est non seulement plus moderne mais supporte également des fichiers plus volumineux, car elle lit le fichier ligne par ligne au lieu de charger l'intégralité du contenu en mémoire d'un seul coup.

## Plongée profonde :
La gestion des opérations sur les fichiers par Go, y compris la lecture de fichiers, reflète la philosophie du langage axée sur la simplicité et l'efficacité. Initialement, le paquet `ioutil` offrait des opérations sur les fichiers simples. Cependant, avec les améliorations de la bibliothèque standard de Go et un virage vers une gestion des erreurs et une gestion des ressources plus explicites, les paquets `os` et `io` sont devenus les alternatives préférées pour travailler avec les fichiers.

Ces changements soulignent l'engagement de Go envers la performance et la sécurité, notamment en évitant les problèmes de mémoire qui peuvent survenir lors du chargement complet de fichiers volumineux. La méthode `bufio.Scanner` introduite pour lire les fichiers ligne par ligne souligne l'adaptabilité du langage et son attention aux défis de l'informatique moderne, tels que le traitement de grands ensembles de données ou les données en flux continu.

Bien qu'il existe des bibliothèques externes disponibles pour travailler avec les fichiers en Go, les capacités de la bibliothèque standard sont souvent suffisantes et préférées pour leur stabilité et performance. Cela assure que les développeurs Go peuvent gérer efficacement les opérations sur les fichiers sans dépendre de dépendances supplémentaires, s'alignant sur l'ethos minimaliste global du langage et son design pour construire des logiciels efficaces et fiables.
