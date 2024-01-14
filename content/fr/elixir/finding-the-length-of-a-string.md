---
title:    "Elixir: Trouver la longueur d'une chaîne de caractères"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

Il est important de pouvoir trouver la longueur d'une chaîne de caractères dans un langage de programmation car cela permet de manipuler et de traiter ces chaînes plus facilement.

## Comment faire

Voici un exemple de code en Elixir pour trouver la longueur d'une chaîne de caractères :

```Elixir
string = "Bonjour"
length = String.length(string)
IO.puts "La longueur de la chaîne de caractères est #{length}"
```

Lorsque vous exécutez ce code, vous devriez obtenir la sortie suivante :

```
La longueur de la chaîne de caractères est 7
```

Cela signifie que la chaîne "Bonjour" a une longueur de 7 caractères, y compris l'espace entre les mots.

## Plongée en profondeur

La fonction `String.length` est une fonction intégrée d'Elixir qui prend en paramètre une chaîne de caractères et renvoie la longueur de cette chaîne. Il est important de noter que cette fonction prend également en compte les caractères unicode, contrairement à d'autres langages de programmation. Cela signifie que même les caractères accentués compteront dans la longueur de la chaîne.

De plus, il existe une autre fonction intégrée en Elixir appelée `String.codepoints` qui permet de compter le nombre de caractères dans une chaîne, plutôt que le nombre de caractères unicode. Cela peut être utile dans certains cas où vous avez besoin de gérer spécifiquement les caractères accentués.

Enfin, il est important de noter que la plupart des fonctions de manipulation de chaînes en Elixir sont fournies par le module `String`, mais vous pouvez également utiliser des fonctions du module `Binary` pour traiter les chaînes de caractères comme des données binaires.

## Voir aussi

- [Documentation officielle d'Elixir sur String.length](https://hexdocs.pm/elixir/String.html#length/1)
- [Documentation officielle d'Elixir sur String.codepoints](https://hexdocs.pm/elixir/String.html#codepoints/1)
- [Documentation officielle d'Elixir sur le module Binary](https://hexdocs.pm/elixir/Binary.html)