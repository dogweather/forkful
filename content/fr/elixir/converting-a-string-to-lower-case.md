---
title:                "Elixir: Conversion d'une chaîne en minuscules"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est la manipulation de chaînes de caractères. Mais parfois, nous devons également les convertir en minuscules pour des raisons telles que la recherche de mots-clés ou la normalisation des données. Dans cet article, nous allons vous montrer comment convertir facilement une chaîne de caractères en minuscules en utilisant le langage de programmation Elixir.

## Comment faire

La conversion d'une chaîne de caractères en minuscules en Elixir est assez simple et peut être réalisée en utilisant la fonction `String.downcase/1`. Voyons un exemple:

```Elixir
string = "Bonjour MONDE"
lowercase_string = String.downcase(string)
IO.puts lowercase_string # Output: bonjour monde
```

Nous pouvons également utiliser la méthode de notation pointée pour appeler la fonction `downcase` directement sur la chaîne de caractères:

```Elixir
string = "Bonjour MONDE"
lowercase_string = string.downcase
IO.puts lowercase_string # Output: bonjour monde
```

Comme nous pouvons le voir, le résultat est une nouvelle chaîne de caractères avec toutes les lettres en minuscules. Mais qu'en est-il des caractères spéciaux? Eh bien, Elixir fait également en sorte que ceux-ci soient convertis correctement. Voyons un exemple avec des accents:

```Elixir
string = "ÉLIXIR est génial!"
lowercase_string = string.downcase
IO.puts lowercase_string # Output: élixir est génial!
```

## Plongée en profondeur

Maintenant que nous savons comment convertir une chaîne de caractères en minuscules, voyons comment cela fonctionne réellement en coulisses. Tout d'abord, il est important de noter que la fonction `String.downcase/1` ne modifie pas la chaîne de caractères d'origine, mais renvoie plutôt une nouvelle chaîne avec les modifications apportées.

En arrière-plan, Elixir utilise le module `String` qui contient plusieurs fonctions utiles pour manipuler les chaînes de caractères. La fonction `downcase/1` utilise la fonction `downcase/2` et la passe à chaque caractère de la chaîne de caractères d'origine en utilisant une boucle récursive. Ainsi, chaque caractère est converti individuellement en minuscule et une nouvelle chaîne est construite en utilisant ces caractères.

Nous pouvons également utiliser `String.downcase/2` pour spécifier la locale à utiliser lors de la conversion en minuscules. Par défaut, la locale utilisée est celle du système, mais nous pouvons la spécifier en utilisant une chaîne de caractères. Par exemple, si nous voulons utiliser la locale fr_FR pour la conversion en minuscules, nous pouvons utiliser la fonction comme ceci:

```Elixir
string = "ÉLIXIR est génial!"
lowercase_string = String.downcase(string, "fr_FR")
IO.puts lowercase_string # Output: Élixir est génial!
```

## Voir aussi

- [Documentation sur la fonction String.downcase/1 d'Elixir](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Guide sur la manipulation des chaînes de caractères en Elixir](https://elixir-lang.org/getting-started/string.html)