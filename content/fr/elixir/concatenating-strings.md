---
title:                "Elixir: Concaténer des chaînes de caractères"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes dans la programmation est la concaténation de chaînes de caractères. Il s'agit simplement de combiner plusieurs chaînes pour en former une seule. Mais pourquoi cela est-il important en Elixir ?

## Comment faire

La concaténation de chaînes en Elixir est très simple et peut être réalisée de différentes manières. Regardons quelques exemples :

```
# Utilisation de l'opérateur `<>`
"Elixir" <> " est génial" # donnera "Elixir est génial"

# Utilisation de la fonction `String.concat()`
String.concat(["Hello", " ", "World"]) # donnera "Hello World"

# Utilisation du pipe operator `|>`
"Bonjour" |> String.replace("Bonjour", "Hello") # donnera "Hello"
```

Comme vous pouvez le voir, Elixir offre différentes solutions pour la concaténation de chaînes, c'est à vous de choisir celle qui convient le mieux à votre style de code.

## Plongée en profondeur

La concaténation de chaînes en Elixir peut sembler simple, mais il est important de comprendre comment cela fonctionne en interne. En fait, Elixir utilise des tuples pour stocker les chaînes de caractères, ce qui signifie que chaque fois que nous concaténons des chaînes, le programme crée un nouveau tuple avec la chaîne combinée à l'intérieur. Cela peut affecter les performances dans les cas où nous concaténons de grandes quantités de chaînes.

Heureusement, Elixir offre une solution pour améliorer les performances en utilisant la fonction `String.Chars.concat()` qui prend en charge la concaténation de plusieurs chaînes sans créer de nouveaux tuples. Cela peut être particulièrement utile si vous travaillez avec des bases de données et que vous devez concaténer plusieurs valeurs avant de les insérer dans une requête SQL.

## Voir aussi

- [La documentation officielle sur la concaténation de chaînes en Elixir](https://hexdocs.pm/elixir/String.html#concatenation)
- [Un article sur les meilleures pratiques en matière de concaténation de chaînes en Elixir](https://alex-min.fr/elixir-string-concatenation/)
- [Un tutoriel vidéo sur la manipulation de chaînes en Elixir](https://www.youtube.com/watch?v=amX5LOUOm1k)