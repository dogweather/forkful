---
title:                "Elixir: Trouver la longueur d'une chaîne"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de manipuler des chaînes de caractères. Savoir la longueur d'une chaîne est donc une compétence essentielle pour tout développeur. Dans cet article, nous allons vous montrer comment trouver la longueur d'une chaîne en utilisant le langage Elixir.

## Comment faire

La méthode la plus simple pour trouver la longueur d'une chaîne est d'utiliser la fonction `String.length/1`. Cette fonction prend en paramètre une chaîne et retourne sa longueur. Voyons un exemple concret :

```Elixir
string = "Bonjour"
String.length(string)
```

Output : `7`

Il est également possible d'utiliser l'opérateur `++` pour concaténer deux chaînes et ainsi obtenir leur longueur combinée. Par exemple :

```Elixir
string1 = "Bonjour"
string2 = "monde"
String.length(string1 ++ string2)
```

Output : `12`

## Plongeon en profondeur

La fonction `String.length/1` utilise un algorithme efficace basé sur la table ASCII pour calculer la longueur d'une chaîne de caractères. Elle parcourt simplement tous les caractères dans la chaîne et compte le nombre de caractères valides. Cependant, il est important de noter que cette fonction peut être moins performante pour les chaînes contenant des caractères Unicode.

Pour ces cas, il existe une autre fonction appelée `String.codepoints/1` qui retourne une liste des points de code Unicode pour chaque caractère dans la chaîne, et ainsi on peut facilement en déduire la longueur de la chaîne. Voici un exemple :

```Elixir
string = "こんにちは"
length = String.codepoints(string) |> Enum.count
IO.puts("La longueur de la chaîne est : #{length}")
```

Output : `La longueur de la chaîne est : 5`

## Voir aussi

Pour en savoir plus sur les opérations sur les chaînes de caractères en Elixir, ces liens peuvent vous être utiles :

- [Documentation officielle sur les chaînes de caractères en Elixir](https://hexdocs.pm/elixir/String.html)
- [Tutoriel sur les chaînes de caractères en Elixir](https://elixirschool.com/fr/lessons/basics/basics/)
- [Guide des chaînes de caractères en Elixir](https://www.codementor.io/ayushchapagain/introduction-to-strings-and-charlists-yh0pbylf1)

N'hésitez pas à les consulter pour améliorer vos compétences en programmation avec Elixir. À bientôt pour d'autres astuces de programmation !