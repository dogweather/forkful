---
title:    "Elixir: Convertir une chaîne en minuscules"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en minuscules peut sembler être une action simple dans la programmation, mais elle peut avoir de nombreux usages pratiques. Cela peut notamment être utile pour comparer des chaînes de caractères de manière insensible à la casse ou pour normaliser les données entrantes.

## Comment faire

Voici un exemple de code en Elixir pour convertir une chaîne de caractères en minuscules :

```elixir
string = "Bonjour Elixir"
lowercase_string = String.downcase(string)
IO.puts lowercase_string
```

Cela produira la sortie suivante :

```
bonjour elixir
```

En utilisant la fonction `String.downcase/1` de la bibliothèque standard d'Elixir, nous pouvons facilement convertir n'importe quelle chaîne de caractères en minuscules.

## Plongée en profondeur

La fonction `String.downcase/1` utilise l'algorithme de pliage de casse (casemap folding) pour effectuer la conversion vers la casse inférieure. En fin de compte, cela signifie que chaque caractère est remplacé par son équivalent en minuscules selon les règles du langage en cours d'utilisation.

Par exemple, lors de l'utilisation de la langue française, la lettre "è" sera convertie en "e" minuscule. Il est important de noter que cela peut varier selon la langue et la configuration du système d'exploitation.

## Voir aussi

- [Documentation sur `String.downcase/1`](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Guide du développeur Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Chaînes de caractères en Elixir : un aperçu](https://www.poignant.guide/book/chapters/string-grammar.html)