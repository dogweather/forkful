---
title:                "Elixir: Extraction de sous-chaînes"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous trouvez face à une chaîne de caractères très longue et vous avez besoin d'en extraire une partie précise ? Ne cherchez plus, extrayez simplement des sous-chaînes en utilisant Elixir !

## Comment faire

Pour extraire une sous-chaîne en Elixir, utilisez la fonction `String.slice` en lui passant en paramètres la chaîne d'origine, l'indice de départ et l'indice de fin de la sous-chaîne souhaitée. Par exemple:

```Elixir
my_string = "Bonjour le monde!"
sub_string = String.slice(my_string, 8, 11)
IO.puts sub_string
# Output: le m
```

Vous pouvez également extraire des sous-chaînes en utilisant des expressions régulières avec la fonction `String.split`. Par exemple:

```Elixir
my_string = "Ceci est un exemple."
sub_string = String.split(my_string, ~r/ est/)
IO.inspect sub_string
# Output: ["Ceci ", " un exemple."]
```

## Plongée en profondeur

Il est important de noter que la fonction `String.slice` utilise l'indexation en zéro, ce qui signifie que le premier caractère d'une chaîne est à l'indice 0. De plus, l'indice de fin n'est pas inclus dans la sous-chaîne résultante.

De plus, Elixir dispose d'autres fonctions utiles pour manipuler les chaînes, telles que `String.replace`, `String.trim`, et bien d'autres encore. Il est recommandé de consulter la documentation officielle pour découvrir toutes les possibilités offertes par Elixir pour traiter les chaînes de caractères.

## Voir aussi

- [Documentation officielle d'Elixir sur les chaînes de caractères](https://hexdocs.pm/elixir/String.html)
- [Blog post "Manipulation des chaînes de caractères en Elixir"](https://spin.atomicobject.com/2020/02/24/string-manipulation-elixir/)
- [Vidéo "Tutoriel Elixir - Manipulation de chaînes de caractères"](https://www.youtube.com/watch?v=c_TeYrX7EJU)