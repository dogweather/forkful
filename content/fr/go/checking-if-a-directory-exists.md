---
title:                "Go: Vérifier l'existence d'un répertoire"
simple_title:         "Vérifier l'existence d'un répertoire"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

La vérification de l'existence d'un répertoire peut sembler être une tâche banale, mais elle est en fait très importante pour une bonne gestion de vos fichiers et de vos programmes en Go. Il est essentiel de s'assurer qu'un répertoire existe avant de pouvoir y créer de nouveaux fichiers ou de l'utiliser pour stocker des données.

# Comment faire

Pour vérifier si un répertoire existe en Go, vous pouvez utiliser la fonction `os.Stat` qui renvoie des informations sur le fichier ou répertoire spécifié. Si le répertoire n'existe pas, une erreur sera renvoyée. Voici un exemple de code montrant comment utiliser cette fonction :

```
package main

import (
	"fmt"
	"os"
)

func main() {
	path := "/chemin/vers/votre/répertoire"

	// Utilisation de la fonction os.Stat pour vérifier si le répertoire existe
	if _, err := os.Stat(path); err == nil {
		fmt.Println("Le répertoire existe")
	} else {
		fmt.Println("Le répertoire n'existe pas")
	}
}
```

Si le répertoire existe, la sortie sera `Le répertoire existe`, sinon elle sera `Le répertoire n'existe pas`.

# Plongée en profondeur

Il est possible de faire une vérification plus détaillée en utilisant la fonction `os.Lstat` qui renvoie une `os.FileInfo` avec des informations supplémentaires sur le fichier ou répertoire spécifié. Vous pouvez également utiliser les méthodes `IsDir()` ou `IsExist()` pour vérifier directement si le chemin pointe vers un répertoire ou si le fichier ou répertoire existe déjà.

# Voir aussi

- [Package os](https://golang.org/pkg/os/)
- [Documentation sur la fonction `os.Stat`](https://golang.org/pkg/os/#Stat)
- [Documentation sur la fonction `os.Lstat`](https://golang.org/pkg/os/#Lstat)