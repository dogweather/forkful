---
title:                "Trouver la longueur d'une chaîne de caractères"
date:                  2024-01-20T17:47:18.230891-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Trouver la longueur d'une chaîne de caractères, c'est simplement compter le nombre de caractères qu'elle contient. Les programmeurs le font pour valider des entrées, optimiser le stockage, ou contrôler la mise en forme.

## Comment faire :
```gleam
import gleam/io

pub fn main() {
  let my_string = "Bonjour!"
  let length = string.len(my_string)
  io.println(length)
}
```
Sortie : `8`

## Plongée en profondeur
Historiquement, la fonction de longueur fait partie des opérations de base sur les chaînes de caractères dans presque tous les langages de programmation. À l'époque des terminaux et des connexions lentes, savoir la taille de la chaîne pouvait être crucial pour la performance. En Gleam, `string.len` est la fonction dédiée pour obtenir cette information. Bien qu'elle soit simple en surface, la complexité peut survenir avec des caractères Unicode, où un seul caractère visuel peut être composé de plusieurs unités de code. Il est important de noter que Gleam manipule les chaînes en UTF-8, ce qui signifie que le nombre renvoyé par `string.len` est le nombre d'unités de code UTF-8, pas nécessairement le nombre de caractères "graphème". Par exemple, un émoji peut compter pour plus d'une unité de code.

## Voir aussi
- Tutoriel sur l'encodage Unicode et UTF-8 : [Understanding Unicode and UTF-8](https://unicode.org/standard/standard.html)
