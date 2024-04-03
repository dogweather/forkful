---
date: 2024-01-20 17:47:19.971983-07:00
description: "How to: En Elixir, la longueur d'une cha\xEEne est trouv\xE9e avec la\
  \ fonction `String.length/1`. Voici comment \xE7a marche ."
lastmod: '2024-03-13T22:44:57.317392-06:00'
model: gpt-4-1106-preview
summary: "En Elixir, la longueur d'une cha\xEEne est trouv\xE9e avec la fonction `String.length/1`."
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
weight: 7
---

## How to:
En Elixir, la longueur d'une chaîne est trouvée avec la fonction `String.length/1`. Voici comment ça marche :

```elixir
string = "Bonjour le monde"
longueur = String.length(string)
IO.puts(longueur)  # Affiche 16
```

Si on utilise des caractères Unicode, ça reste aussi simple :

```elixir
str_unicode = "😀😃😄"
IO.puts(String.length(str_unicode))  # Affiche 3
```

La longueur retourne le nombre de graphèmes, pas nécessairement le nombre d'octets ou de points de code Unicode.

## Deep Dive
Avant Elixir, d'autres langages avaient leurs manières de faire — par exemple, en Python, c'est `len("chaîne")`. Ce qui rend Elixir spécial, c'est qu'il compte les graphèmes. En UTF-8, un caractère comme "é" peut être représenté par plusieurs octets. Elixir gère ça tranquillement.

Alternatives? On pourrait compter les octets (`byte_size/1`) ou les points de code (`String.codepoints/1` et les compter), mais ce n'est pas idéal pour la longueur "visuelle" de la chaîne.

Implementation details? `String.length/1` utilise le module `String.Unicode` d'Elixir pour parcourir correctement les graphèmes. C'est robuste, supportant même les caractères combinants.

## See Also
- [Elixir documentation for String module](https://hexdocs.pm/elixir/String.html)
- [Unicode Standard](http://www.unicode.org/standard/standard.html)
