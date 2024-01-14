---
title:                "Go: Extraction de sous-chaînes"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une pratique courante dans la programmation Go. Elle permet de sélectionner et de manipuler des parties spécifiques d'une chaîne de caractères, ce qui peut être très utile pour traiter des données en entrée ou pour afficher des informations ciblées à l'utilisateur.

## Comment faire

Pour extraire une sous-chaîne en Go, nous utilisons la fonction `Substring` en spécifiant l'index de début et la longueur de la sous-chaîne souhaitée. Voici un exemple de code avec une chaîne de caractères et son résultat :

```
```Go
texte := "Bonjour les amis"
sous_chaine := texte[7:11]
```

Résultat: "les"

Nous pouvons également extraire une sous-chaîne à partir d'une position spécifique jusqu'à la fin d'une chaîne en n'omettant pas le deuxième argument :

```
```Go
texte := "Bonjour les amis"
sous_chaine := texte[7:]
```

Résultat: "les amis"

Cela peut également fonctionner dans le sens inverse en spécifiant uniquement la longueur de la sous-chaîne souhaitée :

```
```Go
texte := "Bonjour les amis"
sous_chaine := texte[:7]
```

Résultat: "Bonjour"

## Plongée en profondeur

La fonction `Substring` utilise des indices 0-based, c'est-à-dire que le premier caractère de la chaîne a un index de 0. De plus, il est important de noter que les indices sont inclusifs pour le premier argument, mais exclusifs pour le deuxième argument.

En utilisant des indices négatifs, nous pouvons également extraire une sous-chaîne à partir de la fin d'une chaîne. Par exemple, la sous-chaîne obtenue en utilisant `texte[:3]` sera équivalente à `texte[:-3]`, car il s'agit des 3 premiers caractères de la chaîne.

Il est également possible d'utiliser la fonction `strings.Index` pour trouver l'index d'un caractère spécifique et l'utiliser ensuite comme argument pour `Substring`.

## Voir aussi

- [Documentation officielle de la fonction `Substring` en Go](https://golang.org/pkg/strings/#Substring)
- [Exemples d'utilisation de la fonction `Substring` en Go](https://gobyexample.com/substrings)
- [Tutoriel sur les chaînes de caractères en Go](https://www.tutorialspoint.com/go/go_string_functions.htm)