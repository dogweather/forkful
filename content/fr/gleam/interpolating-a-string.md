---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Gleam: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Vous avez probablement remarqué que les programmeurs utilisent souvent des chaînes de caractères entourées de caractères spéciaux tels que des "backticks" (`) ou des accolades ({}) dans leur code. Cela s'appelle l'interpolation de chaîne et c'est un moyen pratique de combiner des chaînes et des variables en une seule chaîne de caractères.

Les programmeurs ont recours à l'interpolation de chaîne pour rendre leur code plus lisible et facile à maintenir. Au lieu de concaténer manuellement plusieurs chaînes et variables, ils peuvent simplement les inclure dans une seule chaîne avec des espaces vides et des symboles spéciaux pour les séparer.

## Comment faire:

```
Gleam
fn main() {
  let name = "Jean";
  let greeting = "Bonjour";

  let message = `${greeting} ${name}! Comment ça va?`;

  io::println(message);
}
```

Ce code utilise la syntaxe d'interpolation de chaîne de Gleam pour créer une variable "message" qui contient une chaîne avec le nom et le salut de la personne. Lorsque vous l'exécutez, vous obtiendrez le résultat suivant:

```
Bonjour Jean! Comment ça va?
```

## Plongée en profondeur:

L'interpolation de chaîne existe depuis longtemps dans de nombreux langages de programmation, elle n'est donc pas spécifique à Gleam. Cependant, chaque langage peut avoir sa propre syntaxe pour cela. Par exemple, dans le langage Python, on utilise des f-strings (pour "formatted strings") tandis que dans le langage Ruby, on utilise des chaînes avec des caractères spéciaux tels que #{} pour effectuer une interpolation.

Alternativement, au lieu d'utiliser l'interpolation de chaîne, les programmeurs peuvent concaténer des chaînes à l'aide de fonctions spéciales telles que "join" ou "format". Cependant, ces méthodes peuvent être moins lisibles et nécessitent plus de code.

Au niveau de l'implémentation, l'interpolation de chaîne peut être réalisée en utilisant des expressions régulières ou en parcourant la chaîne et en remplaçant les symboles spéciaux par les valeurs réelles.

## Voir aussi:

Voir la documentation officielle de Gleam sur l'interpolation de chaîne pour plus d'exemples et d'informations.