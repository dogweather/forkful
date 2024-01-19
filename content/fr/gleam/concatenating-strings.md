---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi ? 
La concaténation de chaînes c'est l'action de joindre deux chaînes de caractères pour former une seule chaîne. Les programmeurs le font pour manipuler facilement et efficacement des données textuelles.  

## Comment faire :
Voici comment vous pouvez concaténer des chaînes de caractères en Gleam :

```Gleam
let bienvenue = "Bienvenue à "
let ville = "Paris"
let message = bienvenue <> ville
```

Et le résultat sera 

```Gleam
"Bienvenue à Paris"
```
## Plongée en profondeur
Historiquement parlant, la fonction `<>` en Gleam est inspirée de la fonction de concaténation de chaînes de caractères dans d’autres langages de programmation comme Python ou Javascript. Cependant, Gleam a simplifié cette fonction pour la rendre plus efficace.

En terme d'alternatives, vous pouvez utiliser la fonction `string.concat`, qui est une manière plus formelle de concaténer les chaînes de caractères :

```Gleam
let message = string.concat([bienvenue, ville])
```

En parlant de détails d'implémentation, la concaténation de chaînes en Gleam est bettement efficace. La concaténation de chaînes ne reconstruit pas la chaîne entière, mais crée simplement une nouvelle chaîne qui fait référence aux chaînes source, qui rend la concaténation de chaînes très efficace du point de vue de la mémoire.

## Voir aussi
1. [Documentation officielle imprimante Gleam](https://gleam.run/)
2. [Exemples de code Gleam](https://github.com/gleam-lang/example)
3. [Tutoriel d'introduction à Gleam](https://gleam.run/tour/)