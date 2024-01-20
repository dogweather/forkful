---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi et Quoi?

La recherche et le remplacement de texte sont des opérations couramment effectuées pour modifier des chaînes de caractères. Les programmeurs utilisent ces techniques pour réduire les erreurs et améliorer l'efficacité du code.

## Comment faire:

Gleam rend ces tâches simples. Voici un exemple de recherche et remplacement d'un texte:
```Gleam
let phrase = "J'aime la programmation en Gleam."
let nouvelle_phrase = string.replace(phrase, "Gleam", "Python")

io.println(nouvelle_phrase) // "J'aime la programmation en Python."
```
Dans cet exemple, le texte "Gleam" est recherché et remplacé par "Python" dans la chaîne donnée.

## En Profondeur:

Historiquement, la recherche et le remplacement de textes sont des opérations fondamentales dans le traitement des chaînes de caractères. Beaucoup de langages, comme Perl, Python et même Gleam en sont fortement influencés.

En ce qui concerne les alternatives, vous pouvez également utiliser des expressions régulières pour rechercher et remplacer du texte. Gleam ne supporte pas nativement les expressions régulières, mais il existe des bibliothèques tierces pour cela.

Dans Gleam, `string.replace` est implémenté en utilisant la fonction `string.split` suivie par `list.join`, ce qui n'est pas le moyen le plus efficace. Toutefois, pour la plupart des cas d'utilisation courants, cela devrait suffire.