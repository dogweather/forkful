---
title:                "Concaténation de chaînes de caractères"
html_title:           "Elixir: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faisons-nous?
La concaténation de chaînes consiste à combiner plusieurs chaînes de caractères en une seule chaîne. Les programmeurs le font souvent pour construire des messages ou des données à partir de parties distinctes. Cette opération est commune dans la programmation car elle permet de manipuler et de formater facilement les chaînes.

## Comment faire:
Voici un exemple de concaténation de chaînes en utilisant Elixir:

```Elixir
message1 = "Bonjour"
message2 = "monde!"
message3 = " Comment ça va?"
joined_message = message1 <> " " <> message2 <> message3
IO.puts(joined_message)
```

Résultat:
```
Bonjour monde! Comment ça va?
```

Vous pouvez également utiliser l'opérateur `<>` pour concaténer plusieurs chaînes en une seule, comme dans l'exemple ci-dessus. Il est important de noter que ceci ne modifie pas les chaînes d'origine, mais crée plutôt une nouvelle chaîne à partir des valeurs combinées.

## Plongée en profondeur:
La concaténation de chaînes est une opération de base dans de nombreux langages de programmation. Elle a été introduite dans le langage de programmation C en tant qu'opérateur "+". Cependant, certains langages plus modernes, comme Elixir, ont incorporé un opérateur dédié, `<>`, pour une utilisation plus efficace et lisible.

Il existe également d'autres moyens de concaténer des chaînes dans Elixir, tels que l'utilisation de `String.concat/2` ou `Kernel.<>/2`, qui peuvent être plus performants dans certains cas. En général, il est recommandé d'utiliser l'approche la plus lisible et la plus facile à comprendre pour votre code.

## Voir aussi:
- Documentation sur la concaténation de chaînes en Elixir: https://hexdocs.pm/elixir/String.html#concat/2
- Exemples de concaténation de chaînes avec `String.concat/2`: https://blog.appsignal.com/2018/11/13/how-to-concatenate-strings-in-elixir.html
- Vidéo explicative sur la concaténation de chaînes en Elixir: https://www.youtube.com/watch?v=2LrptYn1aH0