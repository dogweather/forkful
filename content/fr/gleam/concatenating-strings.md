---
title:                "Concaténation de chaînes de caractères"
date:                  2024-01-20T17:34:38.172457-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? - Quoi & Pourquoi ?
Concaténer des chaînes de caractères, c'est les coller bout à bout pour en faire une seule. On fait ça pour assembler du texte de manière dynamique, comme construire des messages personnalisés ou générer du code.

## How to: - Comment faire :
```gleam
fn main() {
  let greeting = "Bonjour"
  let name = "Monde"
  let message = string.concat([greeting, ", ", name, "!"])
  io.println(message)
}
```

Sortie exemple:
```
Bonjour, Monde!
```

## Deep Dive - Plongée en profondeur
Historiquement, la concaténation est essentielle en programmation pour manipuler des textes. En Gleam, on utilise souvent `string.concat(list(strings))` ou l'opérateur `++` pour fusionner des chaînes. Ça diffère des opérations arithmétiques – c'est une opération sur les données textuelles.

Autres méthodes:
- Interpolation: `"Bonjour, \(name)!"`. Plus lisible, surtout pour des chaînes courtes.
- Buffers: pour les manipulations intensives, on optimise avec des buffers.

Concernant l'implémentation, Gleam fait de son mieux pour optimiser la concaténation de chaînes de caractères mais utilisez les buffers pour de meilleures performances avec de grands volumes de texte.
