---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:17.564265-07:00
description: "La cr\xE9ation d'un fichier temporaire en Go permet la g\xE9n\xE9ration\
  \ d'un fichier non persistant destin\xE9 \xE0 une utilisation \xE0 court terme,\
  \ principalement pour\u2026"
lastmod: '2024-03-13T22:44:57.156530-06:00'
model: gpt-4-0125-preview
summary: "La cr\xE9ation d'un fichier temporaire en Go permet la g\xE9n\xE9ration\
  \ d'un fichier non persistant destin\xE9 \xE0 une utilisation \xE0 court terme,\
  \ principalement pour des t\xE2ches telles que le stockage de donn\xE9es provisoires\
  \ ou l'assistance dans des travaux de traitement par lots."
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

## Comment faire :
Dans Go, le paquet `ioutil` fournissait initialement des utilitaires pour la création de fichiers temporaires. Cependant, Go 1.16 a promu l'utilisation des fonctions des paquets `os` et `io/ioutil` dans des emplacements plus organisés. Désormais, les paquets `os` et `io` sont préférés pour la gestion des fichiers temporaires.

Voici un guide étape par étape pour créer, écrire dans, et supprimer un fichier temporaire :

1. **Créer un Fichier Temporaire :**

En utilisant la fonction `os.CreateTemp`, vous pouvez créer un fichier temporaire. Sans spécifier de répertoire, il utilise le dossier temporaire par défaut de votre OS.

```go
package main

import (
    "io/ioutil"
    "log"
    "os"
)

func main() {
    tmpFile, err := ioutil.TempFile("", "exemple.*.txt")
    if err != nil {
        log.Fatal(err)
    }
    log.Printf("Fichier temporaire créé : %s\n", tmpFile.Name())

    defer os.Remove(tmpFile.Name()) // Nettoyage
}
```

2. **Écrire dans le Fichier Temporaire :**

Écrire dans le fichier peut être réalisé avec la méthode `Write` ou d'autres fonctions d'écriture des paquets `io` ou `bufio`.

```go
_, err = tmpFile.Write([]byte("Bonjour, Monde !"))
if err != nil {
    log.Fatal(err)
}
```

3. **Lire depuis le Fichier Temporaire :**

La lecture suit de manière similaire, en utilisant la méthode `Read` du fichier ou en utilisant des utilitaires des paquets `io` ou `bufio`.

```go
data, err := ioutil.ReadFile(tmpFile.Name())
if err != nil {
    log.Fatal(err)
}
log.Printf("Données lues : %s\n", string(data))
```

4. **Supprimer le Fichier Temporaire :**

Alors que l'instruction `defer os.Remove(tmpFile.Name())` à la phase de création assure que le fichier temporaire est supprimé après la fin du programme, la suppression explicite peut être gérée selon les besoins.

Exemple de sortie :
```
2023/04/01 15:00:00 Fichier temporaire créé : /tmp/exemple.123456.txt
2023/04/01 15:00:00 Données lues : Bonjour, Monde !
```

## Approfondissement
Le mécanisme derrière la gestion des fichiers temporaires par Go a évolué. Initialement, la création de fichiers temporaires était majoritairement gérée par la fonction désormais obsolète `ioutil.TempFile`, reflétant les tendances plus larges dans le développement logiciel vers des pratiques de gestion de fichiers plus sécurisées et efficaces. Le passage à l'intégration de ces fonctionnalités dans les paquets `os` et `io` avec Go 1.16 signifie une poussée plus large vers la rationalisation de la bibliothèque standard du langage et l'encouragement de l'utilisation d'API plus unifiées et cohérentes.

Bien que l'utilisation de fichiers temporaires soit une pratique courante et souvent essentielle en programmation, il est important de noter que s'y reposer trop lourdement pour stocker de grandes quantités de données ou pour des tâches à long terme peut conduire à des problèmes de performance. De plus, lorsque la création de fichiers temporaires n'est pas étroitement contrôlée ou lorsqu'ils ne sont pas nettoyés de manière adéquate, cela peut entraîner des fuites de ressources qui pourraient impacter négativement le système de fichiers. Dans des scénarios exigeant un stockage persistant ou nécessitant la gestion de flux de données substantiels, des alternatives telles que les bases de données ou les magasins de données en mémoire offrent souvent de meilleures performances et fiabilité par rapport aux fichiers temporaires.
