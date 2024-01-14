---
title:                "Elixir: Majuscule d'une chaîne de caractères"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La mise en majuscule d'une chaîne de caractères est un moyen courant de formater du texte dans de nombreuses langues de programmation. Dans cet article, nous allons plonger plus en profondeur dans le processus de mise en majuscule des chaînes en utilisant le langage de programmation Elixir.

## Comment faire

Pour mettre en majuscule une chaîne de caractères en Elixir, vous pouvez utiliser la fonction `String.upcase/1`. Elle prend une chaîne de caractères en entrée et renvoie une nouvelle chaîne mise en majuscule.

```Elixir
phrase = "Bonjour le monde!"

resultat = String.upcase(phrase)

IO.puts(resultat) # Affiche "BONJOUR LE MONDE!"
```

Vous pouvez également utiliser la fonction `String.capitalize/1` pour mettre uniquement la première lettre de la chaîne en majuscule.

```Elixir
phrase = "bonjour le monde!"

resultat = String.capitalize(phrase)

IO.puts(resultat) # Affiche "Bonjour le monde!"
```

Enfin, si vous souhaitez mettre en majuscule uniquement la première lettre de chaque mot dans une phrase, vous pouvez utiliser la fonction `String.capitalize_words/1`.

```Elixir
phrase = "bonjour le monde!"

resultat = String.capitalize_words(phrase)

IO.puts(resultat) # Affiche "Bonjour Le Monde!"
```

## Plongée en profondeur

En Elixir, les chaînes de caractères sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées directement. Lorsque vous utilisez les fonctions `String.upcase/1`, `String.capitalize/1` ou `String.capitalize_words/1`, une nouvelle chaîne de caractères est renvoyée à partir de la chaîne d'origine. Cela peut sembler inutile, mais cela permet d'éviter les effets de bord et de garantir l'immutabilité des données.

De plus, ces fonctions s'appuient sur la bibliothèque Unicode pour prendre en charge les caractères non ASCII. Cela signifie que vous pouvez également utiliser ces fonctions pour mettre en majuscule des caractères Unicode spéciaux.

## Voir aussi

- [Documentation officielle sur les chaînes de caractères en Elixir](https://hexdocs.pm/elixir/String.html)
- [Liste des fonctions de la bibliothèque Unicode en Elixir](https://hexdocs.pm/elixir/Unicode.html)