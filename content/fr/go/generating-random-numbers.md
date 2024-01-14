---
title:    "Go: Génération de nombres aléatoires"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Pourquoi

Générer des nombres aléatoires est souvent une nécessité dans la programmation, que ce soit pour un jeu, une simulation ou même pour une couche de sécurité. Apprenons comment le faire en utilisant le langage Go !

## Comment faire

La bibliothèque standard de Go offre plusieurs fonctions pour générer des nombres aléatoires. Voici un exemple simple pour générer un nombre entier aléatoire entre 0 et 100 :

```Go
import (
	"fmt"
	"math/rand"
)

func main() {
	// Génère un nombre aléatoire entre 0 et 100 inclus
	fmt.Println(rand.Intn(101))
}
```

Pour générer un nombre aléatoire dans une plage différente, il suffit d'utiliser une autre fonction. Par exemple, si nous voulons un nombre entier entre 10 et 20, nous pouvons utiliser `Intn` avec une valeur de 10 pour la plage et ajouter 10 au résultat final :

```Go
// Génère un nombre aléatoire entre 10 et 20 inclus
fmt.Println(rand.Intn(11) + 10)
```

Il est également possible de générer des nombres flottants aléatoires en utilisant la fonction `Float64` :

```Go
// Génère un nombre flottant aléatoire entre 0 et 1
fmt.Println(rand.Float64())
// Génère un nombre flottant aléatoire entre 10 et 20
fmt.Println(rand.Float64()*10 + 10)
```

## Plongée profonde

Pour générer des nombres vraiment aléatoires, il est important d'utiliser une source de génération de nombres aléatoires sécurisée. Dans Go, cela peut être réalisé en utilisant un générateur de nombres aléatoires cryptographiquement sécurisé (CSPRNG), tel que `crypto/rand`. Ce générateur utilise les données de l'entropie du système pour générer des nombres aléatoires plus sûrs. 

Il est également important de prendre en compte la séquence de grains dans la génération de nombres aléatoires. En utilisant la même source de grain, les nombres générés seront toujours les mêmes. Pour éviter cela, il est recommandé d'utiliser un générateur de grain différent à chaque exécution.

# Voir aussi

- [La documentation officielle de la bibliothèque standard de Go sur les nombres aléatoires](https://golang.org/pkg/math/rand/)
- [Un article sur les générateurs de nombres aléatoires en Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-math-rand-package-in-go)
- [Un tutoriel vidéo pour générer des nombres aléatoires en Go](https://www.youtube.com/watch?v=fcL259ztVcg)