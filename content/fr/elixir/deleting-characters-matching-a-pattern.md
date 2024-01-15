---
title:                "Suppression de caractères correspondant à un modèle."
html_title:           "Elixir: Suppression de caractères correspondant à un modèle."
simple_title:         "Suppression de caractères correspondant à un modèle."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères dans une chaîne de caractères peut sembler être un petit détail, mais cela peut en fait avoir un impact significatif sur les performances et l'efficacité de votre code. Que ce soit pour optimiser le temps d'exécution ou pour nettoyer des données, la suppression de caractères conformes à un modèle peut être une tâche utile dans de nombreuses situations.

## Comment faire

Voici un exemple de code en Elixir qui supprime tous les caractères non alphabétiques d'une chaîne de caractères :

```Elixir
string = "Elixir 1.10.2"
regexp = ~r/[^a-zA-Z ]/
new_string = Regex.replace(string, regexp, "")
```

La sortie de ce code sera :

```Elixir
"Elixir "
```

Le premier élément à noter est l'utilisation de la fonction `Regex.replace` qui prend en paramètre la chaîne de caractères à traiter ainsi que le modèle de caractères à supprimer, défini par l'opérateur `~r/.../`. Dans cet exemple, le modèle correspond à tous les caractères non alphabétiques ainsi qu'à l'espace. Ce dernier est inclus pour conserver l'espace entre "Elixir" et "1.10.2".

Une autre méthode couramment utilisée pour supprimer des caractères conformes à un certain motif est l'utilisation de la fonction `String.replace`.

## Plongée en profondeur

En y regardant de plus près, la fonction `Regex.replace` utilise en fait la fonction `Regex.replace/3` qui permet de spécifier les options de remplacement. Ces options incluent notamment `all`, qui permet de remplacer toutes les occurrences du modèle, et `global`, qui permet de rechercher dans la chaîne entière plutôt que seulement au début. Vous pouvez consulter la documentation en ligne pour en savoir plus sur les options de remplacement et d'autres fonctions de manipulation de chaîne en Elixir.

## Voir aussi

Voici quelques liens utiles pour continuer à explorer les fonctions de manipulation de chaîne en Elixir :
- [Documentation officielle Elixir sur les chaînes de caractères](https://hexdocs.pm/elixir/String.html)
- [Elixir School : les chaînes de caractères en Elixir](https://elixirschool.com/fr/lessons/basics/strings/)
- [Elixir Forum : discussion sur la manipulation de chaîne dans Elixir](https://elixirforum.com/t/string-manipulation/1103)