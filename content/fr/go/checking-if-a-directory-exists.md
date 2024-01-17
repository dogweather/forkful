---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Go: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?
Vérifier si un répertoire existe est un processus courant dans la programmation pour s'assurer qu'un chemin donné existe et peut être utilisé dans le code. Les programmeurs font cela pour éviter les erreurs ou les problèmes lors de l'exécution de leur programme.

# Comment faire :
```
Go existe le package `os` avec la fonction `Mkdir` pour créer un nouveau répertoire et la fonction `Stat` pour vérifier l'existence d'un répertoire. Voici un exemple de code pour vérifier si un répertoire existe et afficher un message en conséquence :

```Go
import (
	"fmt"
	"os"
)
  
func main() {
  // créer un nouveau répertoire
  err := os.Mkdir("nouveau_repertoire", 0755)
  
  // vérifier s'il y a une erreur
  if err != nil {
    // afficher le message d'erreur
    fmt.Println("Le répertoire existe déjà !")
  } else {
    // si aucun erreur, afficher un message de succès
    fmt.Println("Répertoire créé avec succès !")
  }

  // vérifier si le répertoire existe avant de le supprimer
  if _, err := os.Stat("nouveau_repertoire"); !os.IsNotExist(err) {
    // supprimer le répertoire
    os.Remove("nouveau_repertoire")
    // afficher un message de succès
    fmt.Println("Répertoire supprimé avec succès !")
  }
}
```

# Plongée en profondeur :
La vérification de l'existence d'un répertoire est un processus important pour s'assurer que notre code peut fonctionner correctement. Auparavant, les programmeurs utilisaient la fonction `syscall.Stat` pour vérifier si un répertoire existe, mais elle était limitée car elle ne fonctionnait que sur les systèmes UNIX. Avec le package `os`, nous pouvons vérifier l'existence d'un répertoire à la fois sur les systèmes UNIX et Windows. De plus, en utilisant la fonction `os.Stat`, nous pouvons également obtenir des informations sur un fichier ou un répertoire telles que la taille, les permissions, etc.

# Voir aussi :

- [Package `os` dans la documentation officielle de Go](https://golang.org/pkg/os/)
- [La fonction `Mkdir` dans la documentation officielle de Go](https://golang.org/pkg/os/#Mkdir)
- [La fonction `Stat` dans la documentation officielle de Go](https://golang.org/pkg/os/#Stat)