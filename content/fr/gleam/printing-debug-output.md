---
title:                "Affichage des sorties de débogage"
date:                  2024-01-20T17:52:37.492132-07:00
model:                 gpt-4-1106-preview
simple_title:         "Affichage des sorties de débogage"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? | Quoi et Pourquoi ?
L'impression de débogage consiste à afficher des données pour suivre le flux d'exécution ou les valeurs pendant le développement. On le fait pour traquer les bogues plus facilement et comprendre ce qui se trame dans le programme.

## How to: | Comment faire :
```gleam
import gleam/io

pub fn main() {
  let some_value = 42
  let debug_message = "Current value is: "
  
  // Print to standard output
  io.debug(debug_message ++ int.to_string(some_value))
  
  // Some more code (...)
}
```
Output:
```
Current value is: 42
```
## Deep Dive | Plongée en profondeur :
Historiquement, l'affichage pour le débogage est aussi vieux que la programmation elle-même. Les alternatives modernes incluent l'utilisation d'environnements de développement intégrés (IDE) avec des débogueurs avancés ou la journalisation avec des niveaux de gravité. En Gleam, l'acte d’impression est simple, mais ne doit pas se retrouver dans le code de production. Il peut ralentir les performances et révéler des informations sensibles inopportunément.

## See Also | À voir également :
- Effective debugging strategies in functional programming
- A guide to logging in BEAM languages (Erlang, Elixir, Gleam)