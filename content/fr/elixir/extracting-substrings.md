---
title:    "Elixir: Extraction de sous-chaînes"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elixir, il est très probable que vous ayez déjà eu besoin d'extraire des sous-chaînes à partir d'une chaîne de caractères. Que ce soit pour traiter des données, effectuer des opérations sur des chaînes de caractères spécifiques ou pour des besoins de traitement de texte, l'extraction de sous-chaînes est une tâche commune et utile. Dans cet article, nous allons explorer comment extraire des sous-chaînes en utilisant le langage de programmation Elixir.

## Comment faire

Pour extraire des sous-chaînes en Elixir, nous pouvons utiliser la fonction `String.slice/2`. Cette fonction prend deux paramètres : la chaîne de caractères d'origine et un intervalle de début et de fin pour spécifier la partie de la chaîne à extraire. Voyons un exemple concret en utilisant un code Elixir :

```Elixir
str = "Bonjour le monde !"
String.slice(str, 8..13)
```
Output : `"le monde"`

Nous pouvons également spécifier une seule position dans l'intervalle pour extraire une seule caractère. Par exemple :

```
String.slice(str, 2)
```
Output : `"n"`

En utilisant cette fonction, nous pouvons facilement extraire des parties de chaînes de caractères selon nos besoins.

## Plongée en profondeur 

En plus de la fonction `String.slice/2`, Elixir offre également d'autres méthodes pour extraire des sous-chaînes. Par exemple, nous pouvons utiliser la fonction `String.slice/3` qui prend trois paramètres : la chaîne de caractères d'origine, la position de début et la longueur de la sous-chaîne à extraire. Regardons un exemple :

```Elixir
String.slice("Hello World!", 4, 5)
```
Output : `"o Wor"`

Nous pouvons également utiliser la notation de tranche en utilisant le signe `..` pour définir un intervalle. Par exemple :

```Elixir
str = "Github"
str[2..5]
```
Output : `"ithu"`

L'une des caractéristiques les plus intéressantes de la notation de tranche en Elixir est la prise en charge des index négatifs. Cela signifie que nous pouvons extraire des sous-chaînes à partir de la fin de la chaîne de caractères en utilisant des nombres négatifs pour spécifier la position de début et de fin. Par exemple :

```Elixir
str = "ElixirLang"
String.slice(str, -4..-1)
```
Output : `"Lang"`

En utilisant ces différentes méthodes, nous pouvons extraire des sous-chaînes de manière flexible en fonction de nos besoins.

## Voir aussi

- Documentation sur `String.slice/2` : https://hexdocs.pm/elixir/String.html#slice/2
- Tutorial sur les chaînes de caractères en Elixir : https://elixir-lang.org/getting-started/string.html
- Exemples de tranche en Elixir : https://elixirschool.com/en/lessons/basics/strings/#string-slicing