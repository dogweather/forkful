---
title:    "Gleam: Mettre une chaîne en majuscules"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi

Le fait de mettre en majuscules une chaîne de caractères peut sembler une tâche simple en programmation, mais cela peut avoir un impact important sur l'esthétique et la compréhension de votre code. Dans cet article, nous allons discuter de la raison pour laquelle vous devriez envisager de capitaliser une chaîne de caractères dans vos projets Gleam.

## Comment Faire

Gleam dispose d'une fonction pratique appelée `String.capitalize` qui vous permet de facilement mettre en majuscules la première lettre d'une chaîne de caractères. Voyons comment l'utiliser avec un exemple :

``` Gleam
let name = "gleam"
let capitalized_name = String.capitalize(name)
```

Lorsque nous imprimons `capitalized_name`, nous obtenons `"Gleam"` comme résultat. Comme vous pouvez le voir, la fonction `capitalize` a automatiquement transformé la première lettre en majuscule, sans affecter le reste de la chaîne.

Mais que se passe-t-il si notre chaîne de caractères contient déjà des majuscules ? Dans ce cas, la fonction les laissera en l'état et ne capitalisera que la première lettre. Par exemple :

```Gleam
let phrase = "Bonjour tout le monde"
let capitalized_phrase = String.capitalize(phrase)
```

Dans ce cas, le résultat de `capitalized_phrase` sera `"Bonjour tout le monde"`, car seules la première lettre de la phrase est mise en majuscule.

Il est également possible d'utiliser cette fonction avec des caractères spéciaux comme les accents ou les lettres majuscules accentuées. La fonction les prendra en compte et les laissera tels quels, en ne changeant que la première lettre si nécessaire.

## Plongée en Profondeur

Maintenant que nous avons vu comment utiliser la fonction `capitalize` de Gleam, creusons un peu plus profondément pour comprendre son fonctionnement. En réalité, cette fonction se base sur l'algorithme de transformation de casse Unicode, qui définit toutes les règles pour mettre en majuscule ou en minuscule les caractères de toutes les langues prises en charge.

Ce processus utilise des informations sur les propriétés et les relations entre les caractères pour décider de la transformation finale. Il est donc plus robuste qu'un simple changement de casse selon le code ASCII.

## Voir Aussi

- Documentation de la fonction `String.capitalize` : https://gleam.run/modules/gleam_stdlib/String.html#capitalize
- Algorithme de transformation de casse Unicode : https://unicode.org/faq/casemap_charprop.html