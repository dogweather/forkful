---
title:                "Elixir: Recherche et remplacement de texte"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il est courant de travailler avec de grands volumes de texte et de devoir le modifier ou le remplacer. C'est là que la recherche et le remplacement de texte entrent en jeu. Dans cet article, nous allons explorer comment effectuer cette tâche en utilisant le langage de programmation Elixir.

## Comment faire

La fonction de remplacement de texte en Elixir s'appelle `String.replace/3`. Elle prend trois arguments : la chaîne de caractères initiale, le texte à remplacer et le texte de remplacement. Voici un exemple de son utilisation :

```Elixir
texte_initial = "Bonjour le monde"
texte_remplace = "le monde"
texte_remplacement = "l'univers"

nouveau_texte = String.replace(texte_initial, texte_remplace, texte_remplacement)

IO.puts(nouveau_texte)
```

Ce code va remplacer "le monde" par "l'univers" dans la chaîne de caractères `texte_initial` et imprimer le nouveau texte "Bonjour l'univers". 

Il est également possible d'utiliser des expressions régulières pour effectuer des recherches et remplacements plus complexes. Par exemple, pour remplacer toutes les occurrences de lettres avec des chiffres :

```Elixir
texte = "La réponse est 42"

nouveau_texte = texte
|> String.replace(~r/[a-zA-Z]/, "4")
|> String.replace(~r/ ans /, "4")

IO.puts(nouveau_texte)

# Résultat : "4 4 42"
```

## Plongée profonde

Derrière les coulisses, la fonction `String.replace/3` utilise des fonctions de la bibliothèque standard Elixir telles que `Enum.reduce/3` et `String.upcase/1` pour faire le travail. Cela montre la puissance et la flexibilité du langage Elixir pour gérer les chaînes de caractères.

De plus, pour des recherches et remplacements plus complexes, vous pouvez utiliser la bibliothèque Regex en Elixir pour créer des expressions régulières plus avancées et ainsi améliorer la précision de vos recherches et remplacements.

## Voir aussi

- [Documentation officielle d'Elixir sur les chaînes de caractères](https://hexdocs.pm/elixir/String.html)
- [Documentation officielle d'Elixir sur la bibliothèque Regex](https://hexdocs.pm/elixir/Regex.html)