---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Capitaliser une chaîne de caractères, ça veut dire transformer la première lettre en majuscule. C'est utile pour normaliser l'affichage des noms propres ou des titres.

## Comment faire :

```elixir
defmodule StringCapitalize do
  def capitalize(str) when is_binary(str) do
    String.capitalize(str)
  end
end

IO.puts StringCapitalize.capitalize("elixir") # "Elixir"
IO.puts StringCapitalize.capitalize("bonjour le monde") # "Bonjour le monde"
```

## Plongée en profondeur

La fonction de capitalisation vient de loin. En informatique, ça date des débuts du traitement de texte. En Elixir, on a `String.capitalize/1` - simple et efficace.

Alternatives? Vous pouvez rouler votre propre fonction si vous voulez des règles spéciales. Implémentation? `String.capitalize/1` s'appuie sur les règles Unicode pour la casse, mais attention avec les langues qui n'ont pas de concept de majuscule.

## Voir également

- Elixir `String` module: https://hexdocs.pm/elixir/String.html
- Unicode standard: http://www.unicode.org/reports/tr21/tr21-5.html
