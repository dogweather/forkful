---
title:                "Utiliser les expressions régulières"
html_title:           "Go: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont des outils puissants pour gérer et manipuler du texte dans les programmes Go. Elles permettent aux développeurs de rechercher des motifs spécifiques dans des chaînes de caractères, ce qui est utile pour la validation, la recherche et le remplacement de texte.

## Comment faire

La syntaxe des expressions régulières en Go est similaire à celle d'autres langages tels que Perl et Python. Elles sont disponibles dans le package standard "regexp" et sont utilisées avec la fonction "MatchString". Voici un exemple de code pour trouver toutes les occurrences du mot "Hello" dans une chaîne de caractères :

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    str := "Hello world! Hello from Go!"
    re := regexp.MustCompile("Hello")
    matches := re.FindAllString(str, -1)
    fmt.Println(matches) // Output: [Hello Hello]
}
```

Vous pouvez également utiliser des quantificateurs pour spécifier le nombre de fois qu'un motif doit apparaître dans la chaîne de caractères. Par exemple, si vous voulez trouver des mots comme "Hey" ou "Hello" dans une phrase, vous pouvez utiliser le quantificateur "+" comme ceci :

```Go
re := regexp.MustCompile("He+llo")
matches := re.FindAllString("Heello world! Hello from Go!", -1)
fmt.Println(matches) // Output: [Heello Hello]
```

## Plongée en profondeur

En plus des fonctionnalités de base telles que la recherche de motifs et l'utilisation de quantificateurs, les expressions régulières en Go ont d'autres fonctionnalités intéressantes telles que les groupes de capture et les remplacements. Vous pouvez capturer certains motifs spécifiques en utilisant des parenthèses dans votre expression régulière et accéder à leurs valeurs dans le code. Voici un exemple :

```Go
re := regexp.MustCompile("(He+llo) (world)")
matches := re.FindStringSubmatch("Hello world!")
fmt.Println(matches) // Output: [Hello world Hello world]
fmt.Println(matches[1]) // Output: Hello
fmt.Println(matches[2]) // Output: world
```

Vous pouvez également utiliser la fonction "ReplaceAllString" pour remplacer des parties de la chaîne de caractères avec des valeurs spécifiées. Par exemple, si vous voulez remplacer tous les mots "Hello" par "Bonjour", vous pouvez le faire de cette manière :

```Go
re := regexp.MustCompile("Hello")
str := re.ReplaceAllString("Hello world! Hello from Go!", "Bonjour")
fmt.Println(str) // Output: Bonjour world! Bonjour from Go!
```

## Voir aussi

- La documentation officielle sur les expressions régulières en Go : https://golang.org/pkg/regexp/
- Des exemples de patterns et d'utilisation des expressions régulières en Go : https://github.com/google/re2/wiki/Syntax
- Un tutoriel complet sur les expressions régulières en Go : https://www.geeksforgeeks.org/regular-expressions-in-golang/