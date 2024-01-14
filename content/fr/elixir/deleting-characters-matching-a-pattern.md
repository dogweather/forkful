---
title:    "Elixir: Suppression des caractères correspondant à un modèle"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il y a souvent des tâches répétitives que l'on doit effectuer. Dans certains cas, il peut être utile de supprimer des caractères correspondant à un modèle donné dans une chaîne de caractères. Cela peut sembler simple, mais cela peut créer une différence significative dans la performance et l'efficacité de votre code.

## Comment faire

Pour supprimer des caractères correspondant à un modèle, nous pouvons utiliser la fonction `String.replace/3` en Elixir. Cette fonction prend trois arguments : la chaîne de caractères d'origine, le modèle à rechercher et la chaîne de caractères de remplacement.

Voici un exemple de code :

```elixir
str = "Elixir est un langage de programmation fonctionnelle"
new_str = String.replace(str, "e", "")
IO.puts(new_str)
```

Le résultat de ce code sera : "Elixir st un langag d programmation fonctionnll"

Nous pouvons également utiliser des expressions régulières pour rechercher des motifs plus complexes. Par exemple, pour supprimer tous les signes de ponctuation d'une chaîne de caractères, nous pouvons utiliser la fonction `Regex.replace/3` :

```elixir
str = "Hello, world!"
new_str = Regex.replace(~r/[[:punct:]]/, str, "")
IO.puts(new_str)
```

Le résultat de ce code sera : "Hello world"

## Plongée en profondeur

La fonction `String.replace/3` est très utile, mais elle a quelques limites. Elle ne peut supprimer qu'un seul modèle à la fois et elle ne peut pas remplacer une chaîne de caractères par une autre de longueur différente. Pour surmonter ces limites, nous pouvons utiliser la fonction `String.replace/4`. Cette fonction prend un quatrième argument qui représente le nombre maximal de remplacements à effectuer.

Par exemple, si nous voulons supprimer tous les nombres d'une chaîne de caractères, nous pouvons utiliser la fonction `String.replace/4` de cette façon :

```elixir
str = "12345,67890"
new_str = String.replace(str, ~r/\d/, "", global: true)
IO.puts(new_str)
```

Le résultat de ce code sera : ", "

## Voir aussi

- [Documentation de la fonction `String.replace/3`](https://hexdocs.pm/elixir/String.html#replace/3)
- [Documentation de la fonction `Regex.replace/3`](https://hexdocs.pm/elixir/Regex.html#replace/3)
- [Documentation de la fonction `String.replace/4`](https://hexdocs.pm/elixir/String.html#replace/4)
- [Guide sur les expressions régulières en Elixir](https://elixir-lang.org/getting-started/regex.html)