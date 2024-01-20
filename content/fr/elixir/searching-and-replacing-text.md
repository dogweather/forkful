---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?
La recherche et le remplacement de texte sont des fonctions qui permettent de localiser spécifiquement des éléments dans une chaîne de caractères et de les modifier par un autre texte. Les programmeurs font cela pour la manipulation de données et l'automatisation des tâches de codage répétitives.

## Comment faire :
Pour effectuer une recherche et un remplacement de texte dans Elixir, on utilise généralement la fonction `String.replace/3`. Par exemple,

```Elixir
IO.puts String.replace("Bonjour le monde", "monde", "programmeur")
```

Cela produira :

```
Bonjour le programmeur
```

Sous Elixir, vous pouvez également utiliser les expressions régulières pour une recherche et un remplacement plus sophistiquée. Par exemple,

```Elixir
IO.puts Regex.replace(~r/world/i, "Bonjour le monde", "programmeur")
```

Produira :

```
Bonjour le programmeur
```

## Plongée Profonde
Historiquement, la recherche et le remplacement de texte est une fonctionnalité héritée des éditeurs de texte et des langages de programmation primitifs. Aujourd'hui, sa mise en œuvre dans Elixir est basée sur le module Erlang :binary.

Il existe diverses alternatives pour la recherche et le remplacement de texte dans Elixir:
* Utiliser la fonction `String.split/2` pour diviser la chaîne en une liste de sous-chaînes, effectuer les modifications nécessaires et les réunir avec `Enum.join/2`. 
* Appliquer une approche basée sur des motifs avec `String.splitter/3` et `Enum.map/2`.

La fonction `String.replace/3` d'Elixir utilise un algorithme efficace pour la recherche et le remplacement. Elle scanne la chaîne de gauche à droite, recherche le motif, le remplace par le texte de remplacement et passe au prochain caractère après la fin du texte remplacé.

## Voir Aussi
Pour plus d'informations sur les chaînes de caractères et la manipulation de texte avec Elixir, consultez les ressources suivantes:

* [Documentation officielle Elixir String](https://hexdocs.pm/elixir/String.html)
* [Exploration des chaînes de caractères Elixir](https://elixirschool.com/fr/lessons/basics/strings/)
* [Présentation des expressions régulières Elixir](https://elixirschool.com/fr/lessons/advanced/regular-expressions/)