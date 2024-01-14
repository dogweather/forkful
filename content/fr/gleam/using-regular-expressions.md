---
title:                "Gleam: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi
Les expressions régulières sont un outil puissant pour la manipulation de chaînes de caractères dans le code. Elles permettent de rechercher, de valider et de remplacer des motifs dans du texte, ce qui en fait un outil essentiel pour tout programmeur.

## Comment faire
Pour utiliser des expressions régulières dans Gleam, vous devez d'abord importer le module regex dans votre projet en utilisant la commande `import`. Ensuite, vous pouvez utiliser la fonction `regex.match` pour vérifier si une chaîne de caractères correspond à un motif spécifique.

Voici un exemple de code pour trouver des adresses email dans une chaîne de caractères en utilisant des expressions régulières :

```
Gleam import regex

let email = regex.match("^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,4}$", "example@email.com")

```
L'expression régulière utilisée ici est un motif couramment utilisé pour valider les adresses email. Elle permet de trouver les emails qui respectent une structure spécifique avec une combinaison de lettres, de chiffres, de caractères spéciaux et de domaines.

Lorsque vous exécutez ce code, la variable `email` sera soit `Ok("example@email.com")` si la chaîne de caractères correspond au motif, soit `Err(reason)` si elle ne correspond pas.

## Plongée en profondeur
En plus de la fonction `match`, le module regex de Gleam propose également d'autres fonctions utiles pour travailler avec des expressions régulières telles que `replace`, `findall` et `split`. De plus, Gleam permet également d'utiliser des groupes de capture pour extraire des informations spécifiques d'une chaîne de caractères correspondant à un motif.

Vous pouvez également utiliser des drapeaux pour adapter votre recherche en fonction des besoins, tels que la recherche en ignorant la casse ou en recherchant uniquement à partir d'un certain index.

Il est important de noter que les expressions régulières peuvent être complexes et peuvent causer des erreurs si elles sont utilisées de manière incorrecte. Assurez-vous de bien comprendre les bases des expressions régulières avant de les utiliser dans votre code.

## Voir aussi
- [Documentation officielle de Gleam sur les expressions régulières](https://gleam.run/modules/regex.html)
- [Cheat sheet des expressions régulières en anglais](https://www.debuggex.com/cheatsheet/regex/javascript)
- [Tutoriel sur les expressions régulières en français](https://www.lfd.uci.edu/~gohlke/pythonlibs/#regex)