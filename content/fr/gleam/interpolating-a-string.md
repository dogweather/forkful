---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce & pourquoi ?

L'interpolation de chaînes est une méthode permettant d'insérer des variables à l'intérieur d'une chaîne de caractères. Les programmeurs l'utilisent pour rendre leur code plus lisible et facile à maintenir.

## Comment faire :

Une démonstration simple d'interpolation de chaîne en Gleam ressemblerait à ceci :

```gleam
let nom = "Pierre"
io.println("Bonjour, {nom}!")
```
La sortie serait : `Bonjour, Pierre!`

La balise `{nom}` à l'intérieur de la chaîne est remplacée par la valeur de la variable `nom`.

## Approfondissement :

Historiquement, l'interpolation de chaînes a été introduite pour la première fois dans les langages de script d'Unix dans les années 70. Aujourd'hui, presque tous les langages de programmation modernes, y compris Gleam, prennent en charge cette fonctionnalité pour faciliter le formatage du texte.

Comme alternatives, certains préfèrent utiliser l'opérateur de concaténation ou la fonction `format()` pour combiner les chaînes et les variables. Cependant, l'interpolation est plus concise et plus lisible.

Dans Gleam, l'opérateur d'interpolation `{}` fonctionne en convertissant les variables en chaînes et en les insérer directement dans la chaîne parente. 

## Voir aussi :

Pour en savoir plus sur l'interpolation de chaînes, consultez ces ressources :

1. Documentation de Gleam : https://gleam.run/docs/
2. Guide rapide sur l'interpolation de chaînes : https://www.joezimjs.com/javascript/a-guide-to-string-interpolation/
3. Comparaison de l'interpolation par rapport à d'autres méthodes : https://www.oreilly.com/library/view/you-dont-know/9781491905159/ch04.html