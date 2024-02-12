---
title:                "Mettre en majuscule une chaîne"
aliases: - /fr/elixir/capitalizing-a-string.md
date:                  2024-02-03T19:04:46.891725-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre en majuscule une chaîne"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La capitalisation d'une chaîne de caractères implique de convertir la première lettre de la chaîne en majuscule tout en s'assurant que le reste des lettres est en minuscules. Cette action est couramment nécessaire pour le formatage de l'entrée utilisateur ou l'affichage de texte dans les interfaces utilisateur, où la cohérence et la lisibilité sont importantes.

## Comment faire :

Elixir fournit une manière simple de capitaliser les chaînes de caractères en utilisant ses fonctions intégrées sans nécessiter de bibliothèques tierces. Voici un exemple simple :

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

Sortie :

```
Elixir programming
```

Pour les cas où plus de contrôle ou une logique de capitalisation plus complexe est nécessaire, vous pourriez combiner différentes fonctions de la chaîne. Par exemple, si vous souhaitez capitaliser chaque mot dans une phrase, vous pouvez diviser la phrase en mots, capitaliser chacun, puis les réunir :

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

Sortie :

```
Elixir Is Fun
```

Bien que la bibliothèque standard d'Elixir couvre la plupart des besoins, pour des manipulations de texte plus nuancées, incluant la capitalisation avancée de chaînes de caractères, vous pourriez explorer des bibliothèques tierces telles que Cldr pour l'internationalisation, qui peuvent offrir des comportements de capitalisation spécifiques à chaque locale.
