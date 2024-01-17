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

## Quoi & Pourquoi?
Trouver la longueur d'une chaîne de caractères est une tâche courante dans la programmation Go. Cela consiste à déterminer le nombre de caractères présents dans une chaîne de texte. Les programmeurs font souvent cela pour traiter et manipuler des données textuelles.

## Comment faire:
Il existe plusieurs façons de trouver la longueur d'une chaîne en Go. Voici quelques exemples utilisant la fonction `len()` et la méthode `len()`:

```Go
// Exemple d'utilisation de la fonction `len()` avec une chaîne de caractères
chaîne := "Bonjour!"
longueur := len(chaîne) // longueur est égal à 8

// Exemple d'utilisation de la méthode `len()` avec un tableau de chaînes de caractères
chaines := []string{"Bonjour", "Salut", "Hello"}
longueur := len(chaines) // longueur est égal à 3
```

## Plongée en profondeur:
Trouver la longueur d'une chaîne de caractères peut sembler simple, mais cela a en fait une histoire intéressante. À l'origine, dans les premières versions de Go, il n'y avait pas de fonction `len()` ou de méthode `len()`. Au lieu de cela, les programmeurs utilisaient la boucle `for` pour parcourir la chaîne et compter chaque caractère. Heureusement, avec l'introduction des fonctions et des méthodes, cette tâche est devenue beaucoup plus simple.

Il existe également des alternatives pour trouver la longueur d'une chaîne, telles que les expressions régulières ou l'utilisation de la fonction `RuneCountInString()` pour compter le nombre de runes (caractères Unicode) dans une chaîne.

## Voir aussi:
- Documentation officielle Go sur la fonction `len()` : https://golang.org/pkg/builtin/#len
- Documentation officielle Go sur les slices (incluant la méthode `len()`) : https://blog.golang.org/slices-intro
- Documentation officielle Go sur les expressions régulières : https://pkg.go.dev/regexp
- Tutoriel sur les chaînes en Go : https://www.callicoder.com/strings-in-go/