---
title:                "Génération de nombres aléatoires"
html_title:           "Java: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi générer des nombres aléatoires?

Il existe de nombreuses raisons pour lesquelles vous voudriez générer des nombres aléatoires dans votre programme Java. Peut-être que vous développez un jeu et que vous avez besoin de dés ou de cartes aléatoires pour simuler un aspect du jeu. Ou peut-être que vous construisez un algorithme de chiffrement et que vous avez besoin d'une clé aléatoire pour sécuriser vos données. Quelle que soit la raison, la génération de nombres aléatoires est une compétence utile pour tout programmeur Java.

## Comment le faire?

Générer des nombres aléatoires en Java peut se faire avec la classe `Random`. Voici un exemple de code:

```Java
// Importer la classe Random
import java.util.Random;

// Créer une instance de la classe Random
Random rand = new Random();

// Générer un entier aléatoire de 1 à 10
int num = rand.nextInt(10) + 1;

// Afficher le résultat
System.out.println("Le nombre aléatoire est: " + num);
```

Output:
```
Le nombre aléatoire est: 7
```

Vous pouvez également utiliser la méthode `Math.random()` pour générer des nombres aléatoires en Java, mais cela produira des valeurs de type `double` plutôt que des entiers. Voici un exemple de code utilisant `Math.random()`:

```Java
// Générer un nombre décimal aléatoire entre 0 et 1
double dec = Math.random();

// Afficher le résultat
System.out.println("Le nombre décimal aléatoire est: " + dec);
```

Output:
```
Le nombre décimal aléatoire est: 0.4534985887109746
```

## Plongeons plus profondément

La classe `Random` utilise un algorithme pseudo-aléatoire pour générer des nombres aléatoires. Cela signifie que les valeurs générées ne sont pas vraiment aléatoires, mais elles semblent l'être pour un utilisateur. Pour obtenir une vraie aléatoirité, vous pouvez utiliser la méthode `setSeed()` pour fournir une valeur initiale à l'algorithme, appelée "graine". En utilisant la même graine, vous obtiendrez toujours la même séquence de nombres aléatoires. Vous pouvez également utiliser `nextInt()` avec un argument pour spécifier la limite supérieure que vous voulez pour vos nombres aléatoires.

## Voir aussi

- [Documentation officielle Java sur `Random`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html)
- [Article sur la génération de nombres aléatoires en Java](https://www.baeldung.com/java-generating-random-numbers)