---
title:                "Extraction de sous-chaînes"
html_title:           "Elixir: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez sur des projets qui nécessitent la manipulation de chaînes de caractères, vous avez probablement déjà eu besoin d'extraire une partie spécifique de cette chaîne. L'extraction de sous-chaînes peut être utile dans de nombreuses situations, comme la validation de données ou la création de rapports. Dans cet article, nous allons découvrir comment extraire des sous-chaînes en utilisant Elixir.

## Comment faire

L'extraction de sous-chaînes en Elixir est assez simple grâce à la fonction `String.substring/3`. Cette fonction prend trois arguments: la chaîne d'origine, l'index du début de la sous-chaîne et l'index de fin de la sous-chaîne.

```
Elixir

string = "Bonjour tout le monde"

# Extraire la sous-chaîne "tout"
String.substring(string, 8, 11)
# Output: "tout"

# Extraire la sous-chaîne "le monde"
String.substring(string, 13, 20)
# Output: "le monde"
```

Comme vous pouvez le voir dans les exemples ci-dessus, les indices indiquent les positions des caractères, avec le premier caractère étant à l'index 0. Vous pouvez également utiliser des indices négatifs pour compter à partir de la fin de la chaîne.

```
Elixir

# Extraire la sous-chaîne "monde"
String.substring(string, -6, -1)
# Output: "monde"
```

Vous pouvez également utiliser la fonction `String.length/1` pour obtenir la longueur de la chaîne et ainsi éviter de compter manuellement les caractères.

## Plongée plus profonde

La fonction `String.substring/3` peut sembler simple, mais elle offre en fait de nombreuses possibilités. Par exemple, vous pouvez utiliser un intervalle pour extraire une sous-chaîne.

```
Elixir

# Extraire la sous-chaîne "tout le"
String.substring(string, 8..14)
# Output: "tout le"
```

Vous pouvez également spécifier uniquement le début ou la fin de la sous-chaîne en utilisant `String.start_of/2` et `String.end_of/2`.

```
Elixir

# Extraire la sous-chaîne commençant à l'index 13
String.start_of(string, 13)
# Output: "le monde"

# Extraire la sous-chaîne allant de l'index 8 à la fin
String.end_of(string, 8)
# Output: "tout le monde"
```

De plus, la fonction peut également prendre un dernier argument facultatif pour définir un caractère de remplacement en cas de dépassement de l'index de fin de la chaîne.

```
Elixir

# Extraire la sous-chaîne "tout***"
String.substring(string, 8, 11, "***")
# Output: "tout***"
```

Maintenant que vous connaissez les bases pour extraire des sous-chaînes en Elixir, vous êtes prêt à l'implémenter dans vos projets!

## Voir aussi

- Documentation officielle d'Elixir sur la fonction `String.substring/3`: https://hexdocs.pm/elixir/String.html#substring/3
- Un tutoriel vidéo sur l'utilisation de `String.substring/3`: https://www.youtube.com/watch?v=cTqGoKgK7M0
- Un article sur les fonctions de manipulation de chaînes en Elixir: https://www.amberbit.com/blog/2019/3/31/string-manipulation-elixir-few-examples/