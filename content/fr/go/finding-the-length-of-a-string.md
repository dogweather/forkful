---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Go: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous voulez savoir comment déterminer la longueur d'une chaîne en Go ? Eh bien, c'est une tâche assez commune en programmation. Il est important de comprendre comment le faire pour pouvoir manipuler et traiter correctement les données textuelles dans vos programmes.

## Comment Faire

Pour trouver la longueur d'une chaîne en Go, vous pouvez utiliser la fonction `len`. Cette fonction prend en paramètre une chaîne de caractères et renvoie le nombre de caractères qu'elle contient.

```Go
s := "Hello world"
length := len(s)
fmt.Println(length) // Output: 11
```

Nous déclarons une variable `s` contenant la chaîne de caractères "Hello world". Ensuite, nous utilisons la fonction `len` pour obtenir sa longueur et la stockons dans une autre variable `length`. Enfin, nous imprimons la valeur de `length` et obtenons comme résultat 11.

Vous pouvez également utiliser la boucle `for` et la fonction `range` pour parcourir chaque caractère de la chaîne et incrémenter une variable pour obtenir sa longueur.

```Go
s := "Bonjour"
length := 0

for range s {
    length++
}

fmt.Println(length) // Output: 7
```

Dans cet exemple, nous utilisons la boucle `for` et la fonction `range` pour parcourir chaque caractère de la chaîne `s` et incrémenter la variable `length` à chaque itération. Nous obtenons ainsi la longueur de la chaîne en comptant le nombre de caractères.

## Deep Dive

Il est important de savoir que la fonction `len` renvoie le nombre de caractères de la chaîne et non le nombre d'octets. En effet, en Go, chaque caractère peut être codé sur plusieurs octets en fonction de la méthode d'encodage utilisée.

De plus, la fonction `len` fonctionne également pour d'autres types de données tels que les tableaux, les tranches (slices) et les maps en renvoyant le nombre d'éléments qu'ils contiennent.

## Voir Aussi

- Documentation officielle de Go : https://golang.org/
- Tutoriel officiel sur les chaînes en Go : https://golang.org/doc/effective_go.html#strings 
- Exemples pratiques de manipulation de chaînes en Go : https://gobyexample.com/strings