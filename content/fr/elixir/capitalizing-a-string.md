---
title:    "Elixir: Capitaliser une chaîne de caractères"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Savez-vous que les strings en Elixir sont immuables ? Cela signifie que vous ne pouvez pas modifier une string une fois qu'elle a été créée. Mais parfois, il peut être utile de capitaliser une string pour des raisons esthétiques ou de traitement de données. Dans cet article, nous allons parler de la façon de capitaliser une string en Elixir et pourquoi cela peut être important. 

## Comment faire

Premièrement, nous avons besoin d'une fonction qui prendra en entrée une string et renverra une nouvelle string avec la première lettre en majuscule. Cette fonction peut être codée en une seule ligne en utilisant la fonction `String.capitalize/1` d'Elixir, comme ceci:

```Elixir
my_string = "elixir"
String.capitalize(my_string)
```

Ce qui devrait renvoyer "Elixir" comme output. Mais que se passe-t-il si notre string contient déjà des lettres en majuscule ?

```Elixir
my_string = "eLiXiR"
String.capitalize(my_string)
```

Elixir est intelligent et ne modifiera que la première lettre pour la mettre en majuscule, renvoyant une nouvelle string avec le reste des lettres inchangées. Le résultat sera "ELiXiR". 

Mais que faire si nous voulons capitaliser toutes les lettres de notre string ? Dans ce cas, nous pouvons utiliser la fonction `String.upcase/1` qui renvoie une string entièrement en majuscule. 

```Elixir
my_string = "elixir"
String.upcase(my_string)
```

Va renvoyer "ELIXIR" comme output. 

## Deep Dive

Maintenant que nous avons vu comment capitaliser une string en utilisant les fonctions intégrées d'Elixir, il est important de comprendre que la manipulation de strings peut être complexe. En fonction de la langue utilisée, des caractères spéciaux ou des accents peuvent être présents et doivent être pris en compte lors de la capitalisation d'une string. 

Dans ces cas, il est recommandé d'utiliser la fonction `String.normalize/2` en fournissant le mode "NFD" qui sépare les caractères accentués ou spéciaux en deux parties : la lettre de base et l'accent. Ensuite, vous pouvez utiliser la fonction `String.capitalie/1` ou `String.upcase/1` sur la string normalisée, ce qui permettra de capitaliser correctement les lettres avec accents ou caractères spéciaux. 

## Voir également

- [Documentation officielle d'Elixir sur les strings](https://hexdocs.pm/elixir/String.html)
- [Guide Elixir pour le traitement des strings](https://elixirschool.com/fr/lessons/basics/string/)
- [Tutoriel vidéo sur la manipulation de strings en Elixir](https://www.youtube.com/watch?v=UjvNLcYsXUs)