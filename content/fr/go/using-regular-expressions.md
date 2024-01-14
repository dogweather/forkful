---
title:    "Go: Utilisation des expressions régulières"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières, également connues sous le nom de regex, sont un outil puissant pour manipuler des chaînes de texte dans les programmes Go. Elles permettent de rechercher, de remplacer et de vérifier rapidement des modèles dans une chaîne de caractères. Si vous travaillez avec des données textuelles complexes, l'utilisation des expressions régulières peut grandement faciliter votre travail.

## Comment faire

Pour utiliser les expressions régulières en Go, vous devez importer le package "regexp". Ensuite, vous pouvez utiliser les fonctions de ce package pour créer et manipuler des expressions régulières.

Voici un exemple de code montrant comment rechercher une chaîne de caractères spécifique dans une autre chaîne :

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    str := "Bonjour, je m'appelle Marie et j'ai 27 ans."
    re := regexp.MustCompile("Marie")
    match := re.MatchString(str)
    fmt.Println(match) // affiche true
}
```

Dans cet exemple, la fonction "MatchString" recherche si la chaîne "str" contient le mot "Marie" en utilisant l'expression régulière définie dans "re". Si oui, la variable "match" sera définie sur "true".

Vous pouvez également utiliser les expressions régulières pour remplacer des parties d'une chaîne par une autre valeur. Voici un exemple où nous remplaçons le mot "Marie" par "Jeanne" :

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    str := "Bonjour, je m'appelle Marie et j'ai 27 ans."
    re := regexp.MustCompile("Marie")
    res := re.ReplaceAllString(str, "Jeanne")
    fmt.Println(res) // affiche "Bonjour, je m'appelle Jeanne et j'ai 27 ans."
}
```

Il existe de nombreuses autres fonctions utiles dans le package "regexp" pour manipuler des expressions régulières. N'hésitez pas à les explorer pour en savoir plus.

## Plongée en profondeur

Les expressions régulières peuvent sembler un peu déroutantes au début, surtout si vous n'avez jamais travaillé avec. Pour mieux comprendre leur fonctionnement, il est important de comprendre les différents caractères et symboles utilisés pour créer une expression régulière.

Par exemple, le caractère ".*", également appelé "point-virgule étoilé", est utilisé pour représenter n'importe quel caractère, qu'il soit présent ou non. Ainsi, l'expression régulière "a.*b" recherchera toutes les chaînes de caractères commençant par "a" et se terminant par "b", quel que soit le nombre ou le type de caractères entre les deux.

Il est également possible d'utiliser des "groupes" dans les expressions régulières, qui permettent de diviser une chaîne en sous-chaînes. Les groupes sont définis en utilisant des parenthèses dans l'expression régulière. Par exemple, l'expression "([A-Z]+) ([0-9]+)" permettra de rechercher une chaîne de caractères commençant par une ou plusieurs lettres majuscules, suivies d'un espace, puis de un ou plusieurs chiffres.

Pour en savoir plus sur la syntaxe et les fonctions spécifiques des expressions régulières en Go, n'hésitez pas à consulter la documentation officielle.

## Voir aussi

- Guide de référence des expressions régulières en Go: https://golang.org/pkg/regexp/
- Tutoriel sur les expressions régulières en Go: https://go.dev/play/match.go
- Livre en ligne "Mastering Regular Expressions": https://regex.info/