---
title:    "Go: Convertir une chaîne en minuscules"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Un des aspects les plus importants de la programmation est la manipulation de données. Dans de nombreux cas, les programmes doivent être en mesure de traiter des données de différentes manières pour les rendre plus utiles ou cohérentes. La conversion d'une chaîne de caractères en minuscules est un outil important pour manipuler des données et faciliter leur traitement. 

## Comment faire

```Go
str := "HELLO WORLD"
lowercase := strings.ToLower(str)
fmt.Println(lowercase)
```
```
hello world
```

La librairie standard de Go contient la fonction `strings.ToLower()` qui permet de convertir une chaîne de caractères en minuscules. Cette fonction renvoie une nouvelle chaîne de caractères avec tous les caractères en minuscules. Si la chaîne de caractères contient déjà des caractères en minuscules, ils ne seront pas affectés. La fonction est très facile à utiliser et ne nécessite qu'une seule ligne de code pour convertir une chaîne de caractères en minuscules. 

## Profondeur d'analyse

La conversion d'une chaîne de caractères en minuscules se fait grâce à un processus appelé "normalisation de la casse". Ce processus consiste à changer la représentation visuelle d'une chaîne de caractères en modifiant les majuscules en minuscules. Il existe différentes façons de normaliser la casse, mais en général, la méthode la plus courante et la plus simple est d'utiliser l'algorithme de conversion de la table ASCII. Cet algorithme parcourt la chaîne de caractères et convertit chaque caractère en sa version en minuscules s'il est en majuscule. 

Cette fonction est également utile pour comparer des chaînes de caractères, car elle rend toutes les lettres en minuscules, ce qui élimine toute confusion entre les majuscules et les minuscules lors de la comparaison. De plus, certaines opérations logiques sur les chaînes de caractères telles que la recherche ou le tri peuvent être simplifiées grâce à la conversion en minuscules. 

## Voir aussi

- Documentation officielle sur la fonction `strings.ToLower()` de Go : https://golang.org/pkg/strings/#ToLower
- Tutoriel sur les fonctions de manipulation de chaînes de caractères en Go : https://www.educative.io/edpresso/string-manipulation-in-golang
- Exemples de conversion de chaînes en minuscules avec Go : https://yourbasic.org/golang/convert-string-to-lower-case/