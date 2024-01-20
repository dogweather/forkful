---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Extraire des sous-chaînes implique d'obtenir une partie spécifique ou distincte d'une chaîne de caractères en Elixir. C'est essentiel pour manipuler et travailler efficacement avec des données de texte.

## Comment faire :

Voici un exemple de base de l'extraction de sous-chaînes en Elixir : 

```elixir
chaîne = "Bonjour monde!"
IO.puts String.slice(chaîne, 0..6)
#Sortie : "Bonjour"
```
Dans cet exemple, nous utilisons la fonction `String.slice/2` pour extraire une sous-chaîne. L'index commence à 0, donc `0..6` extrait les 7 premiers caractères.

## Plongée profonde 

Historiquement, l'extraction de sous-chaînes dans Elixir est basée sur le modèle de programmation fonctionnelle utilisé par le langage Erlang sur lequel Elixir est construit. 

En termes d'alternatives, vous pourriez également utiliser `binary_part/3`, une fonction plus bas niveau qui fonctionne aussi bien avec des binaires qu'avec des chaînes de caractères. 

```elixir
chaîne = "Bonjour monde!"
IO.puts :binary.part(chaîne, {0, 7})
#Sortie : "Bonjour"
```

Pour les détails d'implémentation, `String.slice/2` et `:binary.part/3` renvoient une nouvelle chaîne et ne modifient pas l'original, ce qui est conforme au paradigme immuable d'Elixir.

## Voir aussi :

- Documentation officielle Elixir sur les chaînes de caractères : [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Guide sur la manipulation de chaînes de caractères et de binaires en Elixir : [https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)