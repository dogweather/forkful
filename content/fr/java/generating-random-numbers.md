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

## Qu'est-ce que c'est et pourquoi le faisons-nous?

Générer des nombres aléatoires est un moyen pour les programmeurs de produire des nombres aléatoires dans leurs programmes Java. Cela peut être utile pour simuler des situations aléatoires ou pour ajouter de l'incertitude à des jeux ou des algorithmes.

## Comment faire:

Voici un exemple de code en Java pour générer un nombre aléatoire entre 1 et 10 et l'afficher :

```Java
import java.util.Random; // importe la classe Random

Random rand = new Random(); // crée une instance de Random
int randomNumber = rand.nextInt(10) + 1; // génère un nombre aléatoire entre 1 et 10

System.out.println("Le nombre aléatoire est : " + randomNumber); // affiche le nombre aléatoire
```

Voici l'exemple de sortie pour ce code :

```Java
Le nombre aléatoire est : 6
```

## Plongée en profondeur:

Depuis la version 1.0 de Java, la classe Random a été utilisée pour générer des nombres aléatoires. Cependant, à partir de Java 8, la classe Random a été remplacée par une nouvelle classe appelée ThreadLocalRandom, qui offre de meilleures performances lors de la génération de nombreux nombres aléatoires.

Il existe également d'autres options pour générer des nombres aléatoires en Java, telles que la classe SecureRandom pour une plus grande sécurité dans les applications sensibles, ou la méthode Math.random() pour générer des nombres à virgule flottante entre 0 et 1.

La génération de nombres aléatoires en Java est basée sur un algorithme appelé algorithme de Lehmer, qui utilise des opérations mathématiques pour produire des séquences pseudo-aléatoires.

## À voir également:

Pour en savoir plus sur la génération de nombres aléatoires en Java, vous pouvez consulter la documentation officielle de Java sur les classes Random et ThreadLocalRandom :

https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html

https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/concurrent/ThreadLocalRandom.html