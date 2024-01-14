---
title:                "Elixir: Supprimer des caractères correspondant à un motif"
simple_title:         "Supprimer des caractères correspondant à un motif"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi
Êtes-vous un programmeur expérimenté en recherche d'un moyen efficace de supprimer des caractères correspondant à un modèle spécifique dans votre code Elixir ? Ou peut-être êtes-vous juste curieux de savoir comment cette tâche peut être accomplie en utilisant le langage de programmation fonctionnel préféré des développeurs. Quelle que soit la raison, cet article vous expliquera tout ce que vous devez savoir pour supprimer facilement des caractères correspondant à un motif en Elixir.

## Comment faire
Pour supprimer des caractères correspondant à un motif en Elixir, il existe plusieurs options. L'une des méthodes les plus courantes est d'utiliser la fonction `String.replace/3` qui prend en paramètre une chaîne de caractères, un motif et la chaîne de remplacement. Voici un exemple de code pour supprimer les caractères "a" et "e" d'une chaîne de caractères.

```Elixir
string = "Hello World"
new_string = String.replace(string, ~r/[ae]/, "")
IO.puts new_string
# Output: "Hllo World"
```

Dans cet exemple, nous utilisons les expressions régulières pour créer un motif correspondant à toutes les occurrences des lettres "a" et "e" dans la chaîne d'origine. La chaîne de remplacement est simplement une chaîne vide, ce qui signifie que toutes les occurrences du motif seront supprimées dans la chaîne d'origine.

Une autre option est d'utiliser la fonction `String.gsub/3` qui fonctionne de la même manière, mais permet également de spécifier le nombre maximum de remplacements à effectuer.

```Elixir
string = "Hello World"
new_string = String.gsub(string, ~r/[ae]/, "", 1)
IO.puts new_string
# Output: "Hllo World"
```

Ici, nous avons spécifié que nous voulons remplacer seulement une occurrence du motif et donc seule la première lettre "e" sera supprimée de la chaîne d'origine.

## Plongée en profondeur
Si vous souhaitez avoir un meilleur contrôle sur la façon dont les caractères correspondant à un motif sont supprimés, vous pouvez également utiliser la fonction `String.split/3` pour séparer la chaîne en une liste de substrings en utilisant le motif comme séparateur, puis utiliser la fonction `List.flatten/1` pour réduire la liste en une seule chaîne. Voici un exemple de code qui supprime toutes les occurrences du motif "abc" dans une chaîne.

```Elixir
string = "abc123abc456abc"
new_string = string
            |> String.split(~r/abc/)
            |> List.flatten()
IO.puts new_string
# Output: "123456"
```

De plus, il existe également la fonction `Regex.replace/3` qui permet d'utiliser des expressions régulières pour remplacer les occurrences d'un motif dans une chaîne.

## Voir aussi
Maintenant que vous connaissez les bases de la suppression de caractères correspondant à un motif en Elixir, voici quelques articles supplémentaires pour approfondir vos connaissances :

- [Documentation officielle sur les expressions régulières en Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Article sur la manipulation de chaînes en Elixir](https://medium.com/elixirlabs/the-ultimate-elixir-string-manipulation-guide-c08eea4c972f)
- [Exemples de code pratiques pour le traitement de chaînes en Elixir](https://github.com/wilddima/elixir-data-algebra)

N'hésitez pas à explorer et à expérimenter différents moyens de supprimer des caractères correspondant à un motif dans votre code Elixir. Qui sait, vous pourriez découvrir une méthode plus efficace que celles présentées dans cet article. Bon coding !