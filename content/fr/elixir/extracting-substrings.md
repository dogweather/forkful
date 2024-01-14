---
title:    "Elixir: Extraction de sous-chaînes"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une fonctionnalité essentielle de tout langage de programmation. Elle permet de prendre une partie spécifique d'une chaîne de caractères et de la manipuler de manière indépendante. Dans cet article, nous allons plonger dans l'extraction de sous-chaînes en utilisant le langage de programmation Elixir.

## Comment Faire

Dans Elixir, pour extraire une sous-chaîne d'une chaîne donnée, nous pouvons utiliser la fonction `String.slice/3`. Elle prend trois arguments : la chaîne d'origine, l'index de début et l'index de fin de la sous-chaîne que nous voulons extraire.

```Elixir
str = "Bonjour le monde"
String.slice(str, 3, 8)
# Output : "jour l"
```

Nous pouvons également extraire une sous-chaîne en utilisant des nombres négatifs comme indices. Dans ce cas, l'index compte à partir de la fin de la chaîne.

```Elixir
str = "Hello world"
String.slice(str, -5, -1)
# Output : "worl"
```

Pour extraire une sous-chaîne en utilisant des critères spécifiques, nous pouvons utiliser la fonction `String.slice/2` avec des opérateurs logiques. Par exemple, si nous voulons extraire tous les caractères avant le premier espace dans une chaîne, nous pouvons faire :

```Elixir
str = "Ceci est un exemple"
String.slice(str, 0, String.length(str) - 1) |> String.split(" ") |> List.first()
# Output : "Ceci"
```

## Plongée Profonde

Maintenant que nous avons vu comment extraire des sous-chaînes en utilisant des indices définis, voyons comment nous pouvons utiliser des fonctions pour en faire plus. Dans l'exemple précédent, nous avons utilisé `String.split/2` pour découper la chaîne en une liste et `List.first/1` pour récupérer le premier élément de cette liste.

En utilisant ces fonctions, nous pouvons extraire des sous-chaînes basées sur des critères plus complexes, en utilisant par exemple des expressions régulières. Elixir a une librairie de fonctions pour la manipulation de chaînes de caractères appelée `Regex`, qui peut être utilisée dans ces cas-là.

## See Also

- [Documentation officielle de `String.slice/3` en français](https://hexdocs.pm/elixir/String.html#slice/3)
- [Documentation officielle de `Regex` en français](https://hexdocs.pm/elixir/Regex.html)
- [Article en anglais sur l'extraction de sous-chaînes avec Elixir](https://elixircasts.io/extract-substrings-in-elixir)