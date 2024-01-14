---
title:    "Gleam: Trouver la taille d'une chaîne"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche de la longueur d'une chaîne de caractères est une tâche courante dans de nombreux projets de programmation. Cela peut être utile pour de nombreuses raisons, comme la vérification de la validité des données saisies par l'utilisateur, le traitement de fichiers de texte ou la manipulation de données brutes.

## Comment faire

Voici un exemple de code en Gleam pour trouver la longueur d'une chaîne de caractères et afficher le résultat :

``` gleam
let nom = "Gleam"
let longueur = nom |> string.length
io.format("La longueur de la chaîne {} est {}", [nom, longueur])
```

Résultat :

```
La longueur de la chaîne Gleam est 5
```

Nous utilisons la fonction `string.length` pour obtenir la longueur de la chaîne `nom`. Nous pouvons ensuite utiliser la fonction `io.format` pour afficher le résultat avec le texte souhaité.

## Plongée en profondeur

Il est important de garder à l'esprit que la longueur d'une chaîne de caractères peut varier en fonction de l'encodage utilisé. Par exemple, une chaîne de caractères en UTF-8 peut avoir une longueur différente qu'une chaîne en UTF-16. Il est donc essentiel de comprendre le type d'encodage utilisé dans le contexte de votre projet.

De plus, il peut être utile de savoir comment fonctionne réellement la fonction `string.length`. En Gleam, les chaînes de caractères sont en réalité des listes de caractères, ce qui signifie que la fonction `string.length` compte simplement le nombre d'éléments dans la liste.

## Voir aussi

- [Documentation officielle - Typage de chaînes de caractères](https://gleam.run/book/tour/string-types.html)
- [Exemples d'utilisation de la fonction string.length](https://gleam.run/examples/string-length.html)
- [Article de blog - Manipulation de données en Gleam](https://blog.gleam.run/manipulating-data-in-gleam/)