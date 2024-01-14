---
title:    "Gleam: Transformer une chaîne de caractères en minuscules"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en minuscules est une tâche courante dans le développement de logiciels. Cela peut être utile lors de la comparaison de chaînes de caractères ou pour l'affichage uniforme des données. Dans cet article, nous allons expliquer comment effectuer cette tâche en utilisant Gleam.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en utilisant Gleam, nous pouvons utiliser la fonction `String.to_lower/1` (String est le module Gleam pour les opérations sur les chaînes de caractères). Cette fonction prend en paramètre une chaîne de caractères et renvoie une nouvelle chaîne de caractères avec tous les caractères en minuscules. Voyons un exemple concret :

```Gleam
let nom = "GLEAM"
let nomEnMinuscules = String.to_lower(nom)
```

Lorsque nous imprimons la valeur de `nomEnMinuscules`, nous obtenons le résultat suivant :

```Gleam
"gleam"
```

## Plongée en profondeur

Il est important de noter que la méthode `to_lower/1` utilise les règles de l'Unicode pour convertir la chaîne de caractères en minuscules. Cela signifie que même des caractères spéciaux tels que les lettres accentuées seront correctement converties en minuscules. Par exemple, si nous utilisons la chaîne de caractères "ÉLÉGANT" avec la méthode `to_lower/1`, nous obtiendrons le résultat suivant :

```Gleam
"élégant"
```

De plus, si nous voulons conserver la chaîne de caractères d'origine et créer une nouvelle chaîne en minuscules, nous pouvons utiliser la méthode `String.to_lower_copy/1` qui crée une copie de la chaîne de caractères d'origine et la convertit en minuscules.

## Voir aussi

Pour plus d'informations sur les opérations sur les chaînes de caractères en Gleam, vous pouvez consulter les ressources suivantes :

- La documentation officielle de la méthode `String.to_lower/1`
- Le tutoriel Gleam pour les opérations sur les chaînes de caractères
- Un autre article sur la conversion de chaînes de caractères en majuscules en utilisant Gleam.