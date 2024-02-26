---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:21.617718-07:00
description: "Extraire des sous-cha\xEEnes implique de r\xE9cup\xE9rer des portions\
  \ sp\xE9cifiques d'une cha\xEEne en fonction de leurs positions. Les programmeurs\
  \ r\xE9alisent\u2026"
lastmod: '2024-02-25T18:49:54.022101-07:00'
model: gpt-4-0125-preview
summary: "Extraire des sous-cha\xEEnes implique de r\xE9cup\xE9rer des portions sp\xE9\
  cifiques d'une cha\xEEne en fonction de leurs positions. Les programmeurs r\xE9\
  alisent\u2026"
title: "Extraction de sous-cha\xEEnes"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Extraire des sous-chaînes implique de récupérer des portions spécifiques d'une chaîne en fonction de leurs positions. Les programmeurs réalisent fréquemment cette opération pour traiter ou manipuler efficacement des données textuelles, comme l'analyse de l'entrée, la validation de formats ou la préparation de la sortie.

## Comment faire :

En Go, le type `string` est une tranche en lecture seule d'octets. Pour extraire des sous-chaînes, on utilise principalement la syntaxe `slice`, en conjonction avec la fonction intégrée `len()` pour la vérification de la longueur et le paquet `strings` pour des opérations plus complexes. Voici comment vous pouvez y parvenir :

### Découpage de base

```go
package main

import (
    "fmt"
)

func main() {
    str := "Hello, World!"
    // Extrait "World"
    subStr := str[7:12]
    
    fmt.Println(subStr) // Sortie : World
}
```

### Utilisation du paquet `strings`

Pour une extraction de sous-chaînes plus avancée, telles que l'extraction de chaînes après ou avant une sous-chaîne spécifique, vous pouvez utiliser le paquet `strings`.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=John Doe"
    // Extrait la sous-chaîne après "="
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // Sortie : John Doe
}
```

Il est essentiel de noter que les chaînes Go sont encodées en UTF-8 et qu'une tranche d'octets directe peut ne pas toujours aboutir à des chaînes valides si elles incluent des caractères multi-octets. Pour la prise en charge de l'Unicode, envisagez d'utiliser `range` ou le paquet `utf8`.

### Gestion des caractères Unicode

```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    // Trouver la sous-chaîne en considérant les caractères Unicode
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // Sortie : 世界
}
```

## Exploration approfondie

Extraire des sous-chaînes en Go est simple, grâce à sa syntaxe de tranche et sa bibliothèque standard complète. Historiquement, les langages de programmation antérieurs fournissaient des fonctions ou méthodes plus directes pour manipuler de tel texte. Cependant, l'approche de Go met l'accent sur la sécurité et l'efficacité, en particulier avec ses chaînes immuables et la gestion explicite des caractères Unicode à travers les runes.

Bien que le découpage direct bénéficie d'une efficacité en termes de performances, il hérite des complexités de la gestion directe des caractères UTF-8. L'introduction du type `rune` permet aux programmes Go de gérer en toute sécurité le texte Unicode, en faisant une alternative puissante pour les applications internationales.

De plus, les programmeurs venant d'autres langues pourraient regretter l'absence de fonctions intégrées de manipulation de chaînes de haut niveau. Cependant, les paquets `strings` et `bytes` dans la bibliothèque standard de Go offrent un riche ensemble de fonctions qui, tout en nécessitant un peu plus de code standard, fournissent des options puissantes pour le traitement des chaînes, y compris l'extraction de sous-chaînes.

En essence, les choix de conception de Go autour de la manipulation des chaînes reflètent ses objectifs de simplicité, de performance et de sécurité dans le traitement des données textuelles modernes et internationalisées. Bien que cela puisse nécessiter un léger ajustement, Go offre des outils efficaces et efficients pour gérer l'extraction de sous-chaînes et plus encore.
