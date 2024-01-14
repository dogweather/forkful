---
title:                "Swift: Trouver la longueur d'une chaîne de caractères"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous que savoir trouver la longueur d'une chaîne de caractères est une compétence de base pour tout programmeur Swift? Que vous soyez débutant ou expérimenté, être capable de calculer la longueur d'une chaîne de caractères est indispensable pour manipuler les données de manière efficace.

## Comment faire

Calculer la longueur d'une chaîne de caractères en Swift est très simple. Il vous suffit d'utiliser la méthode `count` sur la chaîne de caractères en question. Regardons un exemple concret :

```Swift
let texte = "Bonjour, comment ça va?"
print(texte.count)
```

L'exemple ci-dessus affichera `22` car il y a 22 caractères dans la chaîne de caractères `texte`. Vous pouvez également utiliser cette méthode pour calculer la longueur d'une chaîne de caractères contenue dans une variable ou renvoyée par une fonction.

## Plongée en profondeur

Il est important de noter que la méthode `count` compte les caractères individuels de la chaîne, y compris les espaces et les caractères spéciaux. Elle ne comptera pas les mots ou les symboles.

De plus, cette méthode ne fonctionne que sur des chaînes de caractères. Si vous essayez de l'utiliser sur un autre type de données, comme un entier ou un booléen, vous obtiendrez une erreur.

Pour aller plus loin, vous pouvez également vous intéresser à la méthode `unicodeScalars.count` qui compte le nombre de caractères Unicode dans une chaîne de caractères.

## Voir aussi

Pour en savoir plus sur la manipulation des chaînes de caractères en programmation Swift, vous pouvez consulter les ressources suivantes :

- [Documentation officielle Apple](https://developer.apple.com/documentation/swift/strings)

- [Article Medium sur les chaînes de caractères en Swift](https://medium.com/swift-coding/https-medium-com-swift-coding-4f1e2feeaad1)

- [Vidéo YouTube sur les fonctions de manipulation de chaînes de caractères en Swift](https://www.youtube.com/watch?v=K03b1cmC0H8)