---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La détermination de la longueur d'une chaîne est un élément fondamental de la programmation. Elle mesure combien de caractères sont inclus dans une certaine chaîne. C'est essentiel lorsque vous avez besoin de manipuler ou de valider des données textuelles.

## Comment faire :

Voici comment trouver la longueur d'une chaîne en Elixir :

```Elixir
chaîne = "Bonjour, Monde!"
IO.puts String.length(chaîne)
```

La sortie sera `15`. Ce résultat comprend tous les caractères de la chaîne, y compris les espaces, les virgules et les points d'exclamation.

## Plongeon en profondeur

En parlant d'Elixir, nous ne pouvons pas éviter la mention d'Erlang car Elixir est construit sur la machine virtuelle d'Erlang (BEAM). Donc, quand nous appelons `String.length/1` en Elixir, cela se passe en fait avec `erlang:byte_size/1`.

Par ailleurs, il est important de noter que `String.length/1` compte le nombre de caractères graphiques dans une chaîne, pas le nombre d'octets. Par exemple :

```Elixir
String.length("é") \\ Renvoie 1, même si "é" utilise 2 octets en UTF-8
```

C'est là que Elixir se distingue de la plupart des autres langages qui comptent le nombre d'octets dans une chaîne, pas le nombre de caractères.

## Voir Aussi

Pour plus de détails, consultez la documentation officielle Elixir :
1. [String module](https://hexdocs.pm/elixir/String.html)
2. [Kernel.byte_size/1](https://hexdocs.pm/elixir/Kernel.html#byte_size/1)
3. [String.length/1](https://hexdocs.pm/elixir/String.html#length/1) 

Ces ressources vous donneront plus d'informations sur les opérations de chaîne de caractères en Elixir.