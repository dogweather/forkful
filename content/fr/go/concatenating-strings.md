---
title:                "Go: Assembler des chaînes de caractères"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes, ou la combinaison de plusieurs chaînes en une seule, est un concept fondamental en programmation qui est souvent utilisé pour manipuler et afficher des données. Dans cet article, nous allons explorer comment utiliser cette fonctionnalité en Go et comprendre pourquoi elle est si utile.

## Comment faire

La concaténation de chaînes en Go est assez simple. Tout d'abord, nous devons déclarer les chaînes que nous voulons concaténer en utilisant le type de données `string`. Ensuite, nous pouvons utiliser l'opérateur `+` pour les fusionner.

```Go
// Déclaration des chaînes
prenom := "Jean"
nom := "Dupont"

// Concaténation des chaînes avec l'opérateur +
nomComplet := prenom + " " + nom

// Affichage du résultat
fmt.Println(nomComplet)
// Output: Jean Dupont
```

Il est également possible de concaténer plusieurs chaînes en une seule en utilisant la fonction `fmt.Sprintf()`.

```Go
prenom := "Jean"
nom := "Dupont"
age := 30

// Concaténation de chaînes avec fmt.Sprintf()
presentation := fmt.Sprintf("Bonjour, je m'appelle %s %s et j'ai %d ans.", prenom, nom, age)

// Affichage du résultat
fmt.Println(presentation)
// Output: Bonjour, je m'appelle Jean Dupont et j'ai 30 ans.
```

## Plongée en profondeur

En Go, les chaînes sont des types de données immuables, c'est-à-dire qu'elles ne peuvent pas être modifiées une fois qu'elles ont été créées. Cela signifie qu'à chaque fois que nous concaténons des chaînes, une nouvelle chaîne est créée en mémoire.

Il est donc important de faire attention à la manière dont nous utilisons la concaténation de chaînes, car cela peut avoir un impact significatif sur les performances de notre programme. Dans certains cas, utiliser des méthodes telles que `strings.Join()` peut être plus efficace car elles évitent de créer de nouvelles chaînes à chaque fois.

Aussi, il est important de noter que l'opérateur `+` n'est pas le seul moyen de concaténer des chaînes en Go. Nous pouvons également utiliser la fonction `strings.Join()` ou la méthode `strings.Builder` pour concaténer plusieurs chaînes.

## Voir aussi

- [Documentation officielle sur la concaténation de chaînes en Go](https://golang.org/pkg/strings/#Join)
- [Article sur l'utilisation efficace de la concaténation de chaînes en Go](https://husobee.github.io/golang/string/2015/05/26/concatenate-strings-efficiently.html)
- [Guide sur les meilleures pratiques en matière de performances en Go](https://dave.cheney.net/high-performance-go-workshop/gophercon-2016.html)