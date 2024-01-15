---
title:                "Concaténer des chaînes de caractères"
html_title:           "Elixir: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi
Parfois en programmation, nous avons besoin de combiner plusieurs chaînes de caractères en une seule. Cela peut être utile pour créer des messages d'erreur dynamiques, des journaux de débogage ou simplement pour afficher du texte à l'utilisateur. Dans cet article, nous allons explorer comment concaténer des chaînes de caractères en utilisant Elixir.

## Comment faire
La méthode la plus simple pour concaténer des chaînes de caractères est d'utiliser l'opérateur `<>` entre les deux chaînes que vous souhaitez combiner. Par exemple:

```Elixir
"Il fait " <> "beau aujourd'hui!"
```

Cela retournera la chaîne "Il fait beau aujourd'hui!".

Si vous avez plus de deux chaînes à concaténer, vous pouvez utiliser la fonction `String.join/2`. Elle prend en premier argument une liste de chaînes et en deuxième argument une chaîne de séparation. Par exemple:

```Elixir
String.join(["Bonjour", "le", "monde"], " ")
```

Cela retournera la chaîne "Bonjour le monde".

## Plongée en profondeur
En arrière-plan, l'opérateur `<>` et la fonction `String.join/2` utilisent la fonction `String.concat/2`. Elle prend également une liste de chaînes en premier argument, mais au lieu de spécifier une chaîne de séparation, elle les concatène toutes ensemble. Par exemple:

```Elixir
String.concat(["Le", "beau", "temps"])
```

Cela retournera la chaîne "Lebeautemps". Il est important de noter que cette fonction n'insère pas d'espace entre les chaînes.

De plus, toutes les méthodes mentionnées dans cet article ne modifient pas les chaînes d'origine, mais retournent une nouvelle chaîne. Si vous souhaitez modifier une chaîne existante, vous pouvez utiliser la fonction `String.replace/4` en utilisant une expression régulière pour déterminer l'emplacement où vous voulez insérer une nouvelle chaîne.

## Voir aussi
- [La documentation sur les chaînes de caractères en Elixir](https://hexdocs.pm/elixir/String.html)
- [Un guide pratique pour les opérateurs en Elixir](https://elixir-lang.org/getting-started/operators.html)
- [Une introduction à l'utilisation des expressions régulières en Elixir](https://medium.com/elixirlabs/introduction-to-regular-expressions-in-elixir-eca121f08487)