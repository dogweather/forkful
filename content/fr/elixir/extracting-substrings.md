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

## Quoi et pourquoi?
L'extraction de sous-chaînes est une méthode utilisée par les programmeurs pour récupérer une partie d'une chaîne de caractères plus longue. Elle est souvent utilisée pour analyser des données ou pour traiter des entrées utilisateur. Cela permet aux programmeurs de manipuler des chaînes de caractères plus facilement et efficacement.

## Comment faire:
Voici un exemple d'utilisation de la fonction `String.slice/3` pour extraire une sous-chaîne d'une chaîne de caractères :
```Elixir
str = "Bonjour le monde"
sousChaine = String.slice(str, 8..13)
```
La valeur de la variable `sousChaine` sera "le monde". Vous pouvez également spécifier le début et la fin de la partie de la chaîne à extraire en utilisant la notation `start..finish` ou `start..length`.

## Plongée en profondeur:
L'extraction de sous-chaînes est un concept couramment utilisé dans de nombreuses langues de programmation, notamment en Elixir, en raison de sa simplicité et de son efficacité. Il existe également d'autres méthodes pour extraire des sous-chaînes telles que l'utilisation de regex ou de fonctions de manipulation de listes.

En ce qui concerne l'implémentation en Elixir, la fonction `String.slice/3` utilise le concept d'indexation de chaînes appelé slices pour extraire la partie souhaitée de la chaîne. Cela signifie que l'accès aux éléments d'une chaîne de caractères se fait par leur position plutôt que par leur valeur, ce qui est plus rapide et plus efficace.

## À voir également:
Pour en savoir plus sur l'extraction de sous-chaînes en Elixir, vous pouvez consulter la documentation officielle ainsi que d'autres sources en ligne. Vous pouvez également explorer d'autres fonctions de manipulation de chaînes telles que `String.split/2` pour diviser une chaîne en une liste de sous-chaînes ou `String.replace/3` pour remplacer une partie de la chaîne.