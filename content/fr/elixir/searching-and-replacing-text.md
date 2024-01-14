---
title:    "Elixir: Recherche et remplacement de texte"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches couramment effectuées lors de la programmation en Elixir. Cela peut être utile pour effectuer des modifications rapides dans un grand volume de données ou pour corriger des erreurs dans un code existant. Dans cet article, nous allons explorer comment effectuer ces tâches en utilisant la syntaxe de recherche et de remplacement en Elixir.

## Comment faire

Pour effectuer une recherche et un remplacement de texte en Elixir, nous allons utiliser la fonction `String.replace/4`. Cette fonction prend quatre arguments : la chaîne de caractères dans laquelle rechercher, le motif à rechercher, le motif de remplacement et un compteur optionnel.

```
Elixir
my_string = "Bonjour le monde!"
String.replace(my_string, "Bonjour", "Hello") #=> "Hello le monde!"
```

Dans cet exemple, nous utilisons `String.replace/4` pour remplacer "Bonjour" par "Hello" dans la chaîne de caractères "Bonjour le monde!". Le résultat est "Hello le monde!". Remarquez que seule la première occurrence du motif est remplacée.

Nous pouvons également utiliser des expressions régulières comme motif à rechercher. Par exemple, si nous voulons remplacer tous les nombres dans une chaîne de caractères par un symbole de hashtag, nous pouvons utiliser l'expression régulière `~r/[0-9]+/`.

```
Elixir
my_string = "Il y a 10 chats dans le jardin."
String.replace(my_string, ~r/[0-9]+/, "#") #=> "Il y a # chats dans le jardin."
```

En utilisant des expressions régulières, nous pouvons effectuer des remplacements plus avancés et cibler spécifiquement les motifs que nous voulons remplacer.

## Plongée en profondeur

En plus de `String.replace/4`, il existe d'autres fonctions pour effectuer des recherches et des remplacements en Elixir. Par exemple, `String.replace/3` permet de remplacer toutes les occurrences d'un motif dans une chaîne de caractères, tandis que `String.trim/1` supprime les espaces en début et en fin de chaîne.

Il est également possible de combiner plusieurs fonctions ensemble pour effectuer des transformations plus complexes sur une chaîne de caractères. Par exemple, nous pouvons d'abord utiliser `String.upcase/1` pour mettre en majuscule une chaîne de caractères, puis utiliser `String.replace/4` pour remplacer des lettres spécifiques par d'autres.

## Voir aussi

- [Documentation officielle sur les chaînes de caractères en Elixir](https://hexdocs.pm/elixir/String.html)
- [Expressive Elixir - Replacing Text](https://elixirschool.com/fr/lessons/advanced/pattern-matching/)
- [Syntaxe d'expressions régulières en Elixir](https://hexdocs.pm/elixir/Regex.html)