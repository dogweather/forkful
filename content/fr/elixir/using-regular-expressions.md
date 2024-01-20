---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les expressions régulières (ou regex) sont des séquences de caractères qui forment une recherche de motif. Les programmeurs les utilisent pour manipuler les chaînes de caractères et découvrir des modèles.

## Comment faire :

Elixir, langage fonctionnel conçu pour la tolérance aux pannes et l'exécution simultanée, utilise les regex de manière simplifiée à l'aide du module `Regex`.

```Elixir
# Pour créer une regex :
regex = Regex.compile!("[A-Za-z]")

# Tester une chaîne de caractères avec la regex :
result = Regex.match?(regex, "Bonjour Elixir!")
IO.inspect(result) # => true
```

Ici, nous avons créé une regex pour rechercher n'importe quelle lettre alphabétique. En utilisant `Regex.match?` avec le texte de "Bonjour Elixir!", le programme renvoie `true` car il trouve des correspondances.

## Immersion Profonde 

La regex a été inventée en 1951 par Stephen Cole Kleene, un mathématicien, pour présenter les mathématiques derrière la possibilité des chaînes de symbolisation. Aujourd'hui, elles sont un outil essentiel pour tous les programmeurs.

Il y a plusieurs alternatives à l'utilisation des regex en Elixir, notamment les fonctions intégrées pour les chaînes de caractères comme `String.contains?`, `String.split`, etc. Ces alternatives ne sont pourtant pas aussi flexibles que les regex.

Le module `Regex` d'Elixir utilise le moteur d'expressions régulières PCRE (Perl Compatible Regular Expressions), ce qui signifie que si vous connaissez les regex Perl, elles fonctionneront aussi en Elixir.

## Voir aussi 

- Pour approfondir votre compréhension des regex : [RegexOne](https://regexone.com/)