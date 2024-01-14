---
title:    "Go: Trouver la longueur d'une chaîne"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de manipuler des chaînes de caractères. Mais saviez-vous qu'il est également possible de trouver la longueur d'une chaîne de caractères en Go ? Dans cet article, nous allons expliquer pourquoi il peut être utile de trouver la longueur d'une chaîne et comment le faire en utilisant le langage de programmation Go.

## Comment faire

Pour trouver la longueur d'une chaîne en Go, nous pouvons utiliser la fonction `len()`. Cette fonction prend une chaîne en argument et renvoie la longueur de cette dernière en nombre de caractères. Voyons un exemple :

````Go
// Déclarer une chaîne de caractères
str := "Bonjour le monde !"

// Utiliser la fonction len()
longueur := len(str)

fmt.Println(longueur) // Affiche 19
````

Comme vous pouvez le voir, la variable `longueur` contient désormais la valeur 19, qui correspond à la longueur de la chaîne "Bonjour le monde !".

## Plongeons plus profondément

Il est important de noter que la fonction `len()` renvoie le nombre de caractères de la chaîne, et non le nombre d'octets. En Go, les chaînes de caractères sont encodées en UTF-8 et peuvent donc contenir des caractères composés de plusieurs octets. Par exemple, la chaîne "Bonjour 😊" a une longueur de 11 caractères (et non 9), car le caractère emoji est encodé sur 4 octets en UTF-8.

De plus, la fonction `len()` peut également être utilisée pour trouver la longueur d'autres types de données, comme les tableaux ou les tranches (slices). Elle peut même être utilisée pour obtenir la taille d'un pointeur.

## Voir aussi

- Documentation officielle de la fonction `len()` en Go : https://golang.org/ref/spec#Length_and_capacity
- Tutoriel sur les chaînes de caractères en Go : https://tutorialedge.net/golang/strings-in-go-tutorial/
- Vidéo explicative sur les caractères spéciaux en UTF-8 : https://www.youtube.com/watch?v=MijmeoH9LT4