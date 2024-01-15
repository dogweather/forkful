---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Elixir: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau en Elixir, trouver la longueur d'une chaîne de caractères peut sembler intimidant. Cependant, une fois que vous avez compris comment le faire, cela deviendra un outil précieux pour traiter toute sorte de données.

## Comment Faire

La méthode la plus simple pour trouver la longueur d'une chaîne de caractères est d'utiliser la fonction `String.length()` en lui passant la chaîne de caractères en argument. Voici un exemple de code :

```Elixir
chaine = "Bonjour le monde"
longueur = String.length(chaine)
IO.puts "La longueur de la chaîne est de : #{longueur}"
```

Dans cet exemple, nous définissons d'abord une variable contenant notre chaîne de caractères, puis nous utilisons la fonction `String.length()` pour trouver sa longueur. Enfin, nous affichons la longueur en utilisant la fonction `IO.puts`.

La sortie de ce code sera : `La longueur de la chaîne est de : 17`.

## Plongée en Profondeur

Il est important de noter que la fonction `String.length()` compte chaque caractère de la chaîne, y compris les espaces et les caractères spéciaux. Cela signifie que la longueur d'une chaîne peut être différente de ce que l'on peut s'attendre à voir. Par exemple, la chaîne `"Bonjour, le monde"` aura une longueur de 18, car elle contient un espace et une virgule en plus des 17 caractères de lettres.

De plus, en utilisant Elixir, il est possible d'utiliser des chaînes de caractères en UTF-8, ce qui peut augmenter la longueur de la chaîne. Dans ce cas, la fonction `String.length()` compte le nombre d'octets de la chaîne, pas le nombre de caractères affichés. Il existe une autre fonction, `String.graphemes()`, qui compte le nombre de caractères affichés, mais elle n'est pas encore disponible dans la version actuelle d'Elixir.

## Voir Aussi

Pour en savoir plus sur la manipulation des chaînes de caractères en Elixir, voici quelques liens utiles :

- [Documentation officielle Elixir sur les chaînes de caractères](https://hexdocs.pm/elixir/String.html)
- [Tutoriel sur les chaînes de caractères en Elixir](https://medium.com/@joshuaforrest/how-to-work-with-strings-in-elixir-7a1fe6cafcce)
- [Comparaison de performances de différentes méthodes pour trouver la longueur d'une chaîne de caractères en Elixir](http://andrealeopardi.com/posts/using-elixir-to-build-a-string-length-function-that-is-guaranteed-not-to-crash)