---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ? 

Extraire des sous-chaînes signifie obtenir une portion spécifique d'une chaîne de caractères. Les programmeurs le font pour manipuler des données textuelles en fonction de leurs besoins, comme l'analyse de textes ou la mise en forme des données.

## Comment Faire :

Voici comment extraire des sous-chaînes en Gleam :

```Gleam
let str = "Bonjour le monde"
let sous_chaine = str.slice(0, 7)
```

L'exemple de code ci-dessus extrait la sous-chaîne "Bonjour " de la chaîne "Bonjour le monde".

## Plongée Profonde :

Historiquement, l'extraction de sous-chaînes est un concept introduit avec les langages de programmation de bas niveau comme le C. Dans Gleam, on utilise la fonction `slice(start, end)` pour extraire une sous-chaîne de la chaîne d'origine. C'est une alternative aux méthodes de découpage de chaînes présentes dans d'autres langages de programmation.

Les méthodes du type `substring()` ou `slice()` sont différentes des méthodes comme `split()`, qui divise une chaîne globale selon un délimiteur spécifié. Ces différences sont importantes car elles affectent la manière dont les données textuelles sont manipulées et utilisées.

## Voir Aussi :

Pour plus d'informations sur le traitement des chaînes en Gleam, consultez les liens suivants:

- Documentation officielle de Gleam : [https://gleam.run/documentation](https://gleam.run/documentation)
- Le guide rapide de Gleam: [https://gleam.run/getting-started](https://gleam.run/getting-started)