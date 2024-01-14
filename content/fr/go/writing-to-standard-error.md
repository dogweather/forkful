---
title:                "Go: Écrire vers l'erreur standard"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire des données sur la sortie d'erreur standard est souvent une pratique courante dans la programmation en Go. Cela peut être utilisé pour signaler des erreurs ou des avertissements importants pendant l'exécution du programme.

## Comment faire

Pour écrire sur la sortie d'erreur standard en Go, nous pouvons utiliser la fonction `Fprintf` de la bibliothèque `fmt`. Cette fonction prend en paramètre la sortie d'erreur standard, suivie du format et des valeurs à écrire. Voici un exemple de code :

```
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprintf(os.Stderr, "Une erreur s'est produite : %s\n", "Fatal")
}
```

Lorsque nous exécutons ce programme, nous obtenons la sortie suivante :

```
Une erreur s'est produite: Fatal
```

Nous pouvons également utiliser `os.Stderr` pour écrire directement sur la sortie d'erreur standard sans passer par la fonction `Fprintf`.

## Plongée en profondeur

La sortie d'erreur standard est particulièrement utile pour signaler des erreurs fatales ou des avertissements critiques. Elle peut également être utilisée pour différencier les messages d'erreur des messages de déboguage, qui peuvent être écrits sur la sortie standard.

Cependant, il est important de noter que l'écriture sur la sortie d'erreur standard peut ralentir les performances de l'application. Cela est dû au fait que l'application doit continuellement changer de contexte entre la sortie standard et la sortie d'erreur.

De plus, il est également important d'utiliser les messages d'erreur avec parcimonie et de prendre en compte des alternatives telles que les journaux d'événements ou les rapports d'erreurs pour une gestion plus robuste des erreurs.

## Voir aussi

- [Documentation officielle de Go pour fmt](https://golang.org/pkg/fmt)
- [Un guide pratique pour écrire sur la sortie d'erreur standard en Go](https://www.digitalocean.com/community/tutorials/how-to-write-to-the-error-output-in-go)
- [Un guide complet pour la gestion des erreurs en Go](https://www.ardanlabs.com/blog/2014/10/error-handling-in-go-part-i.html)