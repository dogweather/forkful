---
title:                "Retirer les guillemets d'une chaîne"
date:                  2024-01-26T03:38:27.481072-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Enlever les guillemets d'une chaîne de caractères signifie se défaire de ces enveloppes supplémentaires pour obtenir le texte pur à l'intérieur. Les programmeurs font cela pour assainir l'entrée, éviter les erreurs, et préparer les données pour le traitement où les guillemets sont des nuisances, pas des fonctionnalités.

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
