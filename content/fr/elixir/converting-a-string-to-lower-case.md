---
title:                "Convertir une chaîne en minuscules"
html_title:           "Elixir: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La conversion d'une chaîne de caractères en minuscules est une opération couramment utilisée par les programmeurs pour modifier la forme de leur texte afin de le rendre plus lisible ou de le comparer plus facilement avec d'autres chaînes de caractères. Cela peut également être utile lors de la manipulation de données sensibles à la casse, comme les mots de passe.

## Comment faire:
Voici quelques exemples de code en Elixir pour convertir une chaîne de caractères en minuscules et afficher le résultat:

```Elixir
string = "I Love Elixir"
lowercase_string = String.downcase(string)
IO.puts lowercase_string #=> "i love elixir"
```

Vous pouvez également utiliser la fonction `String.downcase/1` dans une chaîne de caractères en utilisant l'opérateur `|>` pour une notation plus concise:

```Elixir
"I Love Elixir" |> String.downcase() |> IO.puts #=> "i love elixir"
```

## Plongée en profondeur:
Historiquement, la conversion de chaînes en minuscules était souvent utilisée pour faciliter la comparaison de chaînes de caractères indépendamment de leur casse, en évitant les erreurs dûes à une sensibilité à la casse. Cependant, certains langages de programmation modernes, comme Elixir, ont implémenté des structures de données qui gèrent automatiquement ces cas sans avoir besoin de convertir manuellement les chaînes.

Bien que la fonction `String.downcase/1` soit la méthode la plus couramment utilisée pour convertir une chaîne en minuscules en Elixir, il existe d'autres alternatives telles que `String.to_lower/1` ou l'utilisation de fonctions de bibliothèques tierces comme `Stringex`.

## À voir aussi:
- [Documentation officielle d'Elixir sur la fonction String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Documentation officielle d'Elixir sur l'opérateur `|>`](https://hexdocs.pm/elixir/Kernel.html#%7C%3E/2)
- [Bibliothèque Stringex pour manipuler les chaînes de caractères en Elixir](https://github.com/bitwalker/stringex)