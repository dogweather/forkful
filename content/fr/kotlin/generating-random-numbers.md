---
title:                "Kotlin: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi utiliser la génération de nombre aléatoire avec Kotlin ?

La génération de nombre aléatoire est un outil essentiel pour les programmeurs, car elle permet de créer des éléments aléatoires dans un programme. Cela peut être utile pour la génération de données aléatoires à des fins de test, la création de jeux avec des éléments aléatoires ou encore la génération de mots de passe uniques. Dans cet article, nous allons explorer comment générer des nombres aléatoires en utilisant Kotlin.

## Comment faire la génération de nombre aléatoire en Kotlin ?

La bibliothèque de base de Kotlin inclut une fonction ```rand()``` qui permet de générer un nombre aléatoire. Elle prend en paramètre un entier qui représente la limite supérieure du nombre généré. Par exemple, si nous voulons générer un nombre aléatoire compris entre 0 et 9, nous pouvons utiliser la fonction de la manière suivante :

```Kotlin
var randomNumber = rand(10)
```

Ceci générera un nombre aléatoire compris entre 0 (inclus) et 10 (exclu). Pour générer un nombre aléatoire compris entre un minimum et un maximum donné, nous pouvons utiliser la fonction ```nextInt()``` de la classe ```Random```. Par exemple, si nous voulons générer un nombre aléatoire compris entre 50 et 100, notre code ressemblerait à ceci :

```Kotlin
val randomNumber = Random().nextInt(51) + 50
```

La classe ```Random``` inclut également d'autres méthodes utiles pour la génération de nombre aléatoire, telles que ```nextBoolean()```, ```nextDouble()```, ```nextFloat()```, etc.

## Plongée en profondeur : plus d'informations sur la génération de nombre aléatoire

La fonction ```rand()``` de la bibliothèque de base n'est pas réellement aléatoire, car elle utilise un générateur de nombres pseudo-aléatoires. Cela signifie qu'elle utilise une formule mathématique pour générer des nombres qui semblent aléatoires, mais qui sont en fait déterminés par un algorithme. Ainsi, si vous utilisez la fonction plusieurs fois avec la même valeur limite, vous obtiendrez toujours le même résultat.

Pour une génération de nombre aléatoire plus sûre et plus réelle, vous pouvez utiliser la classe ```SecureRandom``` qui utilise une source d'entropie pour générer des nombres aléatoires. Vous pouvez également utiliser des bibliothèques externes telles que Apache Commons pour une génération de nombre aléatoire plus avancée.

# Voir aussi

- [Kotlin Standard Functions](https://kotlinlang.org/docs/reference/scope-functions.html)
- [Documentation officielle de Kotlin sur la bibliothèque de base](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/)
- [Article sur la génération de nombre aléatoire avec Kotlin](https://proandroiddev.com/random-numbers-with-kotlin-9f2846aec9b6)