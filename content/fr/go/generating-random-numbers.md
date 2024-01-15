---
title:                "Générer des nombres aléatoires"
html_title:           "Go: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est un concept fondamental en programmation, qui peut être utilisé dans une grande variété de projets. Que vous cherchiez à créer un jeu, un algorithme de sécurité ou simplement à ajouter un peu de hasard dans votre code, la génération de nombres aléatoires est un outil essentiel.

## Comment faire

La génération de nombres aléatoires en Go est un processus simple et facile à implémenter. Voici quelques exemples de code pour vous montrer comment générer des nombres aléatoires dans votre programme :

```Go
// Générer un nombre aléatoire entre 1 et 10
rand.Seed(time.Now().UnixNano()) // utilise l'heure actuelle comme graine pour la génération aléatoire
num := rand.Intn(10) + 1 // génère un nombre entre 0 et 9, donc on ajoute 1 pour avoir un nombre entre 1 et 10

// Générer un nombre flottant aléatoire entre 0 et 1
rand.Seed(time.Now().UnixNano())
num := rand.Float64() // génère un nombre flottant entre 0 et 1

// Générer un nombre aléatoire dans une plage donnée
rand.Seed(time.Now().UnixNano())
num := 5 + rand.Intn(10) // génère un nombre entre 5 et 14 inclus

// Générer une séquence de nombres aléatoires
rand.Seed(time.Now().UnixNano())
for i := 0; i < 5; i++ {
    fmt.Println(rand.Intn(100)) // génère 5 nombres aléatoires entre 0 et 99 et les affiche à la console
}
```

Voici un exemple de sortie pour les deux premiers exemples de code ci-dessus :

```
7
0.9270447181895963
```

## Plongée en profondeur

Maintenant que vous savez comment générer des nombres aléatoires en Go, il est intéressant de comprendre comment cela fonctionne en interne. En réalité, les ordinateurs génèrent des nombres pseudo-aléatoires en utilisant des algorithmes basés sur une "graine" initiale, qui peut être n'importe quoi, comme une heure ou un nombre choisi par l'utilisateur. C'est pourquoi il est important de régénérer la graine de manière régulière, pour éviter d'obtenir la même séquence de nombres à chaque fois que le programme est exécuté.

Pour générer des nombres vraiment aléatoires, il est possible d'utiliser des packages externes, tels que "crypto/rand", qui utilise des valeurs provenant de sources d'entropie comme le mouvement de la souris ou l'utilisation du clavier.

## Voir aussi

- Documentation officielle de la génération de nombres aléatoires en Go : https://golang.org/pkg/math/rand/
- Package "crypto/rand" : https://golang.org/pkg/crypto/rand/