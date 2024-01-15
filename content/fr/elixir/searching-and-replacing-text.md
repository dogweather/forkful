---
title:                "Recherche et remplacement de texte"
html_title:           "Elixir: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Que vous soyez programmeur ou non, vous avez probablement déjà eu à modifier du texte à un moment ou à un autre. Peut-être que vous avez dû remplacer un mot par un autre dans un document ou dans un site web. Mais saviez-vous que vous pouvez automatiser cette tâche fastidieuse en utilisant Elixir ? Dans cet article, nous allons découvrir comment utiliser Elixir pour effectuer des recherches et des remplacements de texte de manière efficace et rapide.

## Comment Faire
Pour effectuer une recherche et un remplacement de texte en utilisant Elixir, nous pouvons utiliser la fonction `String.replace/3`. Elle prend trois arguments : la chaîne de caractères dans laquelle nous voulons effectuer la recherche, le motif que nous voulons remplacer et le texte de remplacement.

Voici un exemple concret :

```Elixir
texte = "Bonjour tout le monde !"
nouveau_texte = String.replace(texte, "Bonjour", "Salut")
```

Après l'exécution de ces lignes de code, la valeur de `nouveau_texte` sera "Salut tout le monde !". Comme vous pouvez le constater, nous avons remplacé "Bonjour" par "Salut" dans la chaîne de caractères d'origine.

Mais que se passe-t-il si nous voulons remplacer toutes les occurrences d'une certaine sous-chaîne par une autre ? Nous pouvons utiliser la fonction `String.replace_all/4` en fournissant un quatrième argument qui spécifie le nombre maximum de remplacements à effectuer.

```Elixir
texte = "La vie est belle et belle est la vie."
nouveau_texte = String.replace_all(texte, "belle", "merveilleuse", 1)
```

Dans cet exemple, la valeur de `nouveau_texte` sera "La vie est merveilleuse et belle est la vie.". L'utilisation du paramètre `1` permet de remplacer uniquement la première occurrence de "belle" par "merveilleuse".

Vous pouvez également utiliser des expressions régulières pour des recherches et des remplacements plus complexes en utilisant la fonction `Regex.replace/4`.

## Plongée Profonde
Maintenant que vous savez comment effectuer des recherches et des remplacements de base en utilisant Elixir, vous pouvez aller plus loin en explorant les différentes options disponibles. Vous pouvez utiliser des options supplémentaires pour spécifier des conditions de recherche plus spécifiques, telles que l'ignorance de la casse (option `:case_insensitive`), la recherche dans toute la chaîne de caractères (option `global`) ou la spécification de la position de départ (option `{:offset, start}`). Vous pouvez également combiner ces options pour une recherche et un remplacement encore plus précis.

De plus, Elixir offre une grande variété de fonctions de manipulation de chaînes de caractères, telles que `String.trim/1` pour supprimer les espaces en début et fin de chaîne, `String.split/2` pour diviser une chaîne en plusieurs parties en utilisant un délimiteur spécifique, ou encore `String.to_integer/1` pour convertir une chaîne en un entier.

## Voir Aussi
- [La Documentation Elixir sur les Chaînes de Caractères](https://hexdocs.pm/elixir/String.html)
- [Elixir School - Manipulation de Chaînes de Caractères](https://elixirschool.com/fr/lessons/basics/strings/)