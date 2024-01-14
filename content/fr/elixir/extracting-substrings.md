---
title:                "Elixir: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est utile pour manipuler et traiter des chaînes de caractères dans vos programmes Elixir. Cela peut être particulièrement utile pour extraire des informations spécifiques d'une chaîne, comme un nom de fichier ou une date.

## Comment faire

L'extraction de sous-chaînes en Elixir est facile grâce à la fonction `String.slice/3`. Vous pouvez spécifier l'index de début et de fin de la sous-chaîne que vous souhaitez extraire. Voici un exemple de code pour extraire les trois premiers caractères d'une chaîne :

```elixir
str = "Bonjour"
new_str = String.slice(str, 0, 3)
```

Cela renvoie la sous-chaîne "Bon". Vous pouvez également utiliser des index négatifs pour compter à partir de la fin de la chaîne. Par exemple, `String.slice(str, -3, -1)` renvoie la sous-chaîne "our".

Vous pouvez également spécifier un tableau d'index, ce qui vous permet d'extraire plusieurs sous-chaînes à la fois. Par exemple :

```elixir
str = "Hello World"
indexes = [6, -5]
new_str = String.slice(str, indexes)
```

Cela renvoie un tableau avec les sous-chaînes "World" et "Wor".

## Plongée en profondeur

La fonction `String.slice/3` utilise des index Unicode pour gérer les caractères non ASCII. Ainsi, si vous avez des chaînes contenant des caractères spéciaux ou des émojis, cette fonction garantit que les sous-chaînes sont extraites correctement.

De plus, si vous avez besoin de manipuler des sous-chaînes qui ne sont pas délimitées par un nombre fixe de caractères, vous pouvez utiliser la fonction `String.split/2` pour diviser une chaîne en sous-chaînes en utilisant un délimiteur personnalisé.

## Voir aussi

- Documentation officielle Elixir pour String.slice/3 : https://hexdocs.pm/elixir/String.html#slice/3
- Plus d'exemples d'utilisation de l'extraction de sous-chaînes en Elixir : https://elixirschool.com/fr/lessons/basics/binary-and-strings/
- Utilisation des index Unicode en Elixir : https://elixirschool.com/fr/lessons/advanced/unicode/