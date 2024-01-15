---
title:                "Extraction de sous-chaînes"
html_title:           "Ruby: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Extractions de sous-chaînes : Pourquoi vous devriez le faire.

Si vous travaillez régulièrement avec des chaînes de caractères en Ruby, vous pourriez avoir besoin de récupérer une partie spécifique de celle-ci. Cela peut être utile pour une variété de raisons, comme la manipulation de données provenant de fichiers ou de bases de données, ou la recherche de mots-clés dans une chaîne plus longue. Dans cet article, nous allons explorer comment extraire des sous-chaînes en Ruby et pourquoi cela peut être utile.

## Comment faire

Nous allons utiliser une méthode Ruby très utile appelée `slice`, qui nous permet d'extraire une partie d'une chaîne de caractères en spécifiant l'index de début et de fin de la sous-chaîne que nous voulons extraire. Voici un exemple de code :

```Ruby
phrase = "Je suis un article de blog écrit en Ruby"
puts phrase.slice(11,5)
```
Output: article

Dans cet exemple, nous utilisons `slice` pour extraire les cinq caractères à partir de l'indice 11 de la chaîne `phrase`.

Nous pouvons également utiliser des index négatifs pour compter à partir de la fin de la chaîne. Voici un autre exemple :

```Ruby
nom = "Jeanne Dupont"
puts nom.slice(0..4)
```
Output: Jeanne

Dans cet exemple, nous utilisons un index de `0..4` pour extraire les cinq premiers caractères de `nom`.

Nous pouvons également spécifier un seul index pour extraire à partir d'un point spécifique de la chaîne jusqu'à la fin. Voici un dernier exemple :

```Ruby
mot = "crocodile"
puts mot.slice(0..2)
```
Output: cro

Dans cet exemple, nous utilisons un index de `0..2` pour extraire les trois premiers caractères de `mot`.

## Plongée en profondeur

Il est important de noter que `slice` ne modifie pas la chaîne originale, elle retourne plutôt une nouvelle chaîne contenant la partie extraitée. De plus, si l'index de fin spécifié est hors de la plage de la chaîne, `slice` retournera simplement les caractères jusqu'à la fin de la chaîne.

En plus de `slice`, Ruby possède d'autres méthodes pour extraire des sous-chaînes, comme `scan`, qui renvoie un tableau de toutes les correspondances trouvées dans une chaîne ou `split`, qui sépare une chaîne en plusieurs parties basées sur un séparateur spécifié.

Enfin, il est important de noter que les indices en Ruby commencent à zéro et non à un comme dans d'autres langages de programmation, donc il peut être utile de consulter la documentation pour des méthodes spécifiques lorsque vous travaillez avec des string et des index.

## Voir aussi

- [Documentation officielle Ruby pour `slice`](https://ruby-doc.org/core-3.0.2/String.html#method-i-slice)
- [Blog de Rubyist sur l'extraction de sous-chaînes en Ruby](https://rubyist.to/extracting-substrings-in-ruby/)