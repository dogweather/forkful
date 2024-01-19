---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Convertir une chaîne de caractères en minuscules signifie changer toutes les lettres majuscules d'une chaîne en lettres minuscules. Les programmeurs font cela pour normaliser les données, ce qui facilite la comparaison et le tri des chaînes de caractères.

## Comment faire :

Voici comment convertir une string en minuscules en Elixir.

```Elixir
IO.puts String.downcase("Bonjour Le Monde")
```
Output
```
"bonjour le monde"
```

Et voilà! Votre string est maintenant convertie en lower case.

## Des Détails Plus Profonds

Historiquement, la conversion des chaînes de caractères en minuscules est une pratique courante dans de nombreux langages de programmation, y compris Elixir. Comme Elixir est sensible à la casse, la normalisation facilite la manipulation des strings.

Il y a des alternatives pour changer la casse d'une chaîne dans Elixir, comme l'utilisation de la méthode `String.swapcase/2` pour inverser la casse de chaque lettre dans la chaîne de caractères.

```Elixir
IO.puts String.swapcase("Bonjour Le Monde")
```
Output
```
"bONJOUR lE mONDE"
```

La conversion des chaînes de caractères à minuscules est relativement simple en Elixir grâce à la méthode intégrée `String.downcase/1`.

## Voir Aussi :

1. Documentation Elixir pour [String.downcase](https://hexdocs.pm/elixir/String.html#downcase/2)
2. Documentation Elixir pour [String.swapcase](https://hexdocs.pm/elixir/String.html#swapcase/2)