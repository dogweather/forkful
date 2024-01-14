---
title:                "Gleam: Convertir une chaîne en minuscules"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est parfois nécessaire de convertir une chaîne de caractères en minuscules. Cela peut être utile pour comparer des mots sans tenir compte de la casse ou pour une meilleure lisibilité. Dans cet article, nous allons expliquer comment effectuer cette conversion en utilisant le langage de programmation Gleam.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en utilisant Gleam, nous allons utiliser la fonction `String.to_lowercase`. Cette fonction prend une chaîne en argument et renvoie une nouvelle chaîne en minuscules.

Voici un exemple de code :

```Gleam
let exemple = "BONJOUR";
let conversion = String.to_lowercase(exemple);
```

La variable `conversion` contiendra maintenant la chaîne "bonjour". Vous pouvez également utiliser cette fonction directement sur une chaîne littérale :

```Gleam
let resultat = String.to_lowercase("PROGRAMMATION");
```

Le résultat sera "programmation".

## Plongée en profondeur

Il est important de noter que la fonction `String.to_lowercase` utilise l'encodage UTF-8. Cela signifie que les caractères spéciaux et les accents seront également convertis en minuscules. Par exemple, la chaîne "ÉLÉPHANT" sera convertie en "éléphant".

De plus, la conversion en minuscules peut différer selon la langue utilisée. Par exemple, en français, la lettre "I" majuscule est convertie en "i" minuscule avec un accent aigu, tandis que la lettre "I" majuscule en anglais sera simplement convertie en "i" minuscule sans accent.

Il est également possible de convertir une chaîne en majuscules en utilisant la fonction `String.to_uppercase`.

## Voir aussi

- Documentation officielle de Gleam : https://gleam.run/
- Guide de démarrage rapide : https://gleam.run/getting-started/
- Exemples de code : https://github.com/gleam-lang/gleam/tree/master/examples