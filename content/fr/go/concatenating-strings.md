---
title:                "Concaténation de chaînes de caractères"
html_title:           "Go: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Sincèrement, as-tu déjà eu besoin de combiner plusieurs morceaux de texte pour en faire un seul ? Si oui, alors tu devrais savoir à quel point cette tâche peut être fastidieuse et répétitive. Heureusement, Go (la version actuelle) offre une solution simple et efficace pour concaténer des chaînes de caractères.

## Comment faire

Pour concaténer des chaînes de caractères en Go, tu peux utiliser l'opérateur de concaténation `+` ou la fonction `strings.Join()`. Regardons un exemple:

```Go
nom := "Jean"
age := 30

// Avec l'opérateur de concaténation
phrase := "Bonjour, je m'appelle " + nom + " et j'ai " + string(age) + " ans."
fmt.Println(phrase) // "Bonjour, je m'appelle Jean et j'ai 30 ans."

// Avec la fonction strings.Join()
morceaux := []string{"Bonjour, je m'appelle", nom, "et j'ai", string(age), "ans."}
phrase := strings.Join(morceaux, " ")
fmt.Println(phrase) // "Bonjour, je m'appelle Jean et j'ai 30 ans."
```

Dans cet exemple, nous avons d'abord déclaré deux variables contenant notre nom et notre âge. Ensuite, nous avons utilisé l'opérateur de concaténation pour créer une phrase en combinant ces valeurs avec des chaînes de caractères littérales. Enfin, nous avons utilisé la fonction `strings.Join()` en lui passant un tableau de chaînes de caractères et un séparateur pour créer la même phrase.

## Plongée en profondeur

En Go, les chaînes de caractères sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées une fois qu'elles ont été créées. Ainsi, lorsqu'on concatène des chaînes de caractères, on crée en réalité une nouvelle chaîne de caractères contenant la combinaison des différentes chaînes.

Il est important de noter que l'utilisation répétée de l'opérateur `+` pour concaténer des chaînes de caractères peut entraîner une baisse de performance, car à chaque concaténation, une nouvelle chaîne de caractères est créée. Il est donc recommandé d'utiliser la fonction `strings.Join()` lorsque l'on a besoin de concaténer un grand nombre de chaînes de caractères.

## Voir aussi

- [La documentation officielle de Go sur les chaînes de caractères](https://golang.org/pkg/strings/)
- [Un tutoriel sur Go pour les débutants](https://gobyexample.com/)