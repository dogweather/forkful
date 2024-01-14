---
title:                "Elixir: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de la recherche et du remplacement de texte est un aspect essentiel de la programmation en Elixir. Cela permet de modifier et de mettre à jour rapidement et facilement du texte dans les fichiers de code, ce qui peut être particulièrement utile lors de la refacturation de grandes bases de code. Dans cet article, nous allons explorer comment effectuer des recherches et des remplacements de texte en Elixir.

## Comment faire

Pour effectuer des recherches et des remplacements de texte en Elixir, nous pouvons utiliser la fonction `String.replace/4`. Elle prend quatre arguments : la chaîne de caractères initiale, la chaîne à rechercher, la chaîne de remplacement et des options de recherche. Voici un exemple de code :

```Elixir
phrase = "Bonjour le monde!"
nouvelle_phrase = String.replace(phrase, "Bonjour", "Salut")
IO.puts(nouvelle_phrase)
```

Cet exemple nous donne la sortie suivante : `Salut le monde!`. Comme vous pouvez le constater, la fonction `String.replace` a remplacé "Bonjour" par "Salut" dans la chaîne initiale.

Il est également possible de spécifier des options de recherche pour une recherche et un remplacement plus précis. Par exemple, en utilisant l'option `:global`, la fonction remplacera toutes les occurrences de la chaîne à rechercher dans la chaîne initiale.

## Plongée en profondeur

En plus de la fonction `String.replace/4`, Elixir offre également la fonction `String.replace_all/4`, qui fonctionne de la même manière, mais prend en charge des expressions régulières pour la chaîne à rechercher.

De plus, Elixir dispose également de la macro `sigil_r/2` qui peut être utilisée pour effectuer des recherches et des remplacements de texte en utilisant une syntaxe plus claire.

## Voir aussi

- Documentation sur `String.replace`: https://hexdocs.pm/elixir/String.html#replace/4
- Documentation sur `String.replace_all`: https://hexdocs.pm/elixir/String.html#replace/4
- Documentation sur `sigil_r`: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#sigil_R/2

Merci d'avoir lu cet article sur la recherche et le remplacement de texte en Elixir ! N'hésitez pas à explorer ces différentes options et à les utiliser dans votre code pour rendre votre travail plus efficace et plus agréable.