---
date: 2024-01-26 03:38:27.481072-07:00
description: "Comment faire : Elixir n'a pas de fonction int\xE9gr\xE9e pour 'enlever\
  \ les guillemets', mais c'est un jeu d'enfant de cr\xE9er la v\xF4tre avec la correspondance\
  \ de\u2026"
lastmod: '2024-03-13T22:44:57.314320-06:00'
model: gpt-4-0125-preview
summary: "Elixir n'a pas de fonction int\xE9gr\xE9e pour 'enlever les guillemets',\
  \ mais c'est un jeu d'enfant de cr\xE9er la v\xF4tre avec la correspondance de motifs\
  \ ou les fonctions `String`."
title: "Retirer les guillemets d'une cha\xEEne"
weight: 9
---

## Comment faire :
Elixir n'a pas de fonction intégrée pour 'enlever les guillemets', mais c'est un jeu d'enfant de créer la vôtre avec la correspondance de motifs ou les fonctions `String`. Voyez ces extraits :

```elixir
# Utilisation de la correspondance de motifs
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Exemple d'utilisation
unquote_string("\"Bonjour, Monde !\"") # => "Bonjour, Monde !"
unquote_string("'Bonjour, Monde !'")   # => "Bonjour, Monde !"

# Utilisation de String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Exemple d'utilisation
unquote_string("\"Bonjour, Monde !\"") # => "Bonjour, Monde !"
unquote_string("'Bonjour, Monde !'")   # => "Bonjour, Monde !"
```

Le résultat pour les deux méthodes sera :
```
"Bonjour, Monde !"
```

## Plongée en profondeur
Autrefois, les guillemets dans les chaînes étaient un champ de mines : les manipuler de manière inappropriée, et boum, erreurs de syntaxe ou failles de sécurité. Dans Elixir, la correspondance de motifs traite vos chaînes comme des blocs de Lego, vous permettant de les décomposer et de les reconstruire avec précision. Son robuste module `String` s'avère également pratique, éliminant souplement les guillemets avec les fonctions `trim`. Les alternatives ? Les expressions régulières peuvent envoyer valser les guillemets, et des bibliothèques externes pourraient offrir une puissance supplémentaire si vous avez besoin de plus qu'un simple décapage.

## Voir aussi
Approfondissez avec ceux-ci :
- [Le module String d'Elixir](https://hexdocs.pm/elixir/String.html)
- [En savoir plus sur la correspondance de motifs dans Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Les expressions régulières dans Elixir (module Regex)](https://hexdocs.pm/elixir/Regex.html)
