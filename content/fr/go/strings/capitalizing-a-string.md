---
title:                "Mettre une chaîne en majuscules"
aliases: - /fr/go/capitalizing-a-string.md
date:                  2024-02-03T17:52:37.674346-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre une chaîne en majuscules"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Capitaliser une chaîne de caractères consiste à transformer en majuscule le premier caractère d'une chaîne donnée s'il est en minuscule, pour s'assurer que la chaîne se démarque ou respecte certaines normes grammaticales. Les programmeurs effectuent fréquemment cette opération pour formater des entrées d'utilisateur, afficher des noms propres ou garantir la cohérence des données à travers des applications logicielles.

## Comment faire :

En Go, le package `strings` ne fournit pas de fonction directe pour capitaliser uniquement la première lettre d'une chaîne. Par conséquent, nous combinons la fonction `strings.ToUpper()`, qui convertit une chaîne en majuscules, avec du découpage pour atteindre notre objectif. Voici comment faire :

```go
package main

import (
    "fmt"
    "strings"
    "unicode/utf8"
)

func CapitalizeFirst(str string) string {
    if str == "" {
        return ""
    }
    // Vérifier si le premier caractère est déjà en majuscule.
    if utf8.ValidString(str) && unicode.IsUpper([]rune(str)[0]) {
        return str
    }
    
    // Convertir le premier caractère en majuscule
    r, size := utf8.DecodeRuneInString(str)
    return string(unicode.ToUpper(r)) + str[size:]
}

func main() {
    example := "hello, World!"
    fmt.Println(CapitalizeFirst(example)) // Sortie : "Hello, World!"
}
```

Cette fonction vérifie si la chaîne est vide ou si le premier caractère est déjà en majuscule. Elle utilise le package `unicode/utf8` pour gérer correctement les caractères Unicode, s'assurant que notre fonction fonctionne avec une large gamme d'entrées au-delà du basic ASCII.

## Exploration détaillée

Le besoin de capitaliser des chaînes en Go sans fonction intégrée pourrait sembler être une limitation, en particulier pour les programmeurs venant de langages où les fonctions de manipulation de chaînes sont plus complètes. Cette contrainte encourage la compréhension de la manipulation des chaînes et l'importance de l'Unicode dans le développement logiciel moderne.

Historiquement, les langages de programmation ont évolué dans leur traitement des chaînes, les premiers langages négligeant souvent l'internationalisation. L'approche de Go, bien que nécessitant un peu plus de code pour des tâches apparemment simples, garantit que les développeurs sont attentifs aux utilisateurs mondiaux dès le départ.

Il existe des bibliothèques en dehors de la bibliothèque standard, comme `golang.org/x/text`, offrant des capacités de manipulation de texte plus sophistiquées. Cependant, utiliser celles-ci devrait être pesé par rapport à l'ajout de dépendances externes à votre projet. Pour de nombreuses applications, les packages `strings` et `unicode/utf8` de la bibliothèque standard fournissent des outils suffisants pour une manipulation des chaînes efficace et efficiente, comme montré dans notre exemple. Cela maintient les programmes Go légers et maintenables, faisant écho à la philosophie du langage de la simplicité et de la clarté.
