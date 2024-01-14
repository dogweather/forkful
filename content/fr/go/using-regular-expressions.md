---
title:    "Go: Utiliser des expressions régulières"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi utiliser des expressions régulières en Go ?

Les expressions régulières sont des outils puissants pour le traitement de chaînes de caractères en Go. Elles permettent de définir des motifs et de rechercher, extraire ou remplacer des données dans une chaîne. Si vous avez besoin de manipuler des données complexes ou de faire des recherches précises dans du texte, les expressions régulières peuvent être un moyen efficace d'y parvenir.

## Comment utiliser des expressions régulières en Go ?

Pour utiliser des expressions régulières en Go, vous devez importer le package `regexp`. Ensuite, vous pouvez créer une expression régulière en utilisant la fonction `Compile` et en lui passant le motif que vous souhaitez rechercher. Par exemple, pour trouver toutes les adresses e-mail dans une chaîne, vous pouvez utiliser l'expression régulière `([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,})`. Ensuite, vous pouvez utiliser les méthodes `Match` ou `FindAllString` pour rechercher tous les résultats correspondants à l'expression régulière dans une chaîne de caractères. Voici un exemple de code:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {

    // Créer l'expression régulière 
    re := regexp.MustCompile(`([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,})`)

    // Chaîne de caractères à rechercher
    str := "Mon adresse e-mail est john.doe@example.com et j'utilise également jdoe98@gmail.com"

    // Rechercher toutes les adresses e-mail correspondantes
    matches := re.FindAllString(str, -1) // -1 pour trouver toutes les correspondances

    // Afficher les résultats
    for _, match := range matches {
        fmt.Println(match)
    }
}
```

Le résultat de ce code sera:

```Go
john.doe@example.com
jdoe98@gmail.com
```

## Plongée en profondeur dans l'utilisation des expressions régulières en Go

En plus de rechercher des motifs dans du texte, les expressions régulières en Go offrent également la possibilité de capturer des parties spécifiques d'une chaîne. Pour cela, on peut utiliser des groupes de captures en utilisant les parenthèses dans l'expression régulière. Par exemple, si on souhaite capturer le nom d'utilisateur et le domaine d'une adresse e-mail, on peut modifier l'expression régulière précédente pour devenir `([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+\.[a-zA-Z]{2,})`. En utilisant la méthode `FindStringSubmatch` au lieu de `FindAllString`, on obtiendra un tableau avec le nom d'utilisateur et le domaine en tant que sous-chaînes. Voici un exemple de code:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {

    // Créer l'expression régulière 
    re := regexp.MustCompile(`([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+\.[a-zA-Z]{2,})`)

    // Chaîne de caractères à rechercher
    str := "Mon adresse e-mail est john.doe@example.com et j'utilise également jdoe98@gmail.com"

    // Rechercher toutes les adresses e-mail correspondantes avec les groupes de captures
    matches := re.FindStringSubmatch(str)

    // Afficher les résultats
    for i, match := range matches {
        fmt.Printf("Groupe %d: %s\n", i, match)
    }
}
```

Le résultat de ce code sera:

```Go
Groupe 0: john.doe@example.com
Groupe 1: john.doe
Groupe 2: example.com
```

## Voir aussi

- [Documentation officielle de l'utilisation des expressions régulières en Go](https://golang.org/pkg/regexp/)
- [Guide de référence pour les expressions régulières en Go](https://github.com/google/re2/wiki/Syntax)
- [Tutorial interactif pour apprendre à utiliser les expressions régulières en Go](https://regex-golang-debugger.com/)